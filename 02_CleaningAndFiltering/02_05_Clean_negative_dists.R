#### Data cleaning to address issues with occurrences outside ranges -----------

library(dplyr)
library(readr)
library(sf)

#### Get data ------------------------------------------------------------------
## Read in edge dist data
setwd("data/processed/Edge_dist_data")
data <- data.frame()
data_species <- data.frame()
for (file in list.files(pattern = ".csv")){
  data_species <- read_csv(file)
  data <- rbind(data, data_species)
}

setwd("C:/Users/tasha/OneDrive - Imperial College London/PhD/PROJECT_RangeEdgeType")


## how many species and studies
length(unique(data$Final_name))
length(unique(data$Study))



# convert distance to km
data$nearest_edge_dist_km <- data$nearest_edge_dist / 1000
data$nearest_non_coastal_edge_dist_km <- data$nearest_non_coastal_edge_dist / 1000

# look at distances
hist(data$nearest_non_coastal_edge_dist_km)
summary(data$nearest_non_coastal_edge_dist_km)


data <- as.data.frame(data)

## I'm considering anything >200km outside range to be problematic
negatives <- subset(data, nearest_non_coastal_edge_dist_km < -200)
acceptable <- subset(data,nearest_non_coastal_edge_dist_km > -200)


#### Remove species that are _only_ found >200km outside their range -----------

# There are clear taxonomic issues with these species
all_outside_range <- negatives[which(negatives$Final_name %in% acceptable$Final_name == FALSE),]

# how many species is that?
length(unique(all_outside_range$Final_name))
unique(all_outside_range$Final_name)

# look at their total number of presences 
all_outside_range_counts <- all_outside_range %>% group_by(Final_name) %>% dplyr::summarise(total_presences = sum(Occupancy))
print(all_outside_range_counts, n = nrow(all_outside_range_counts))

# remove these species
data <- data[which(data$Final_name %in% all_outside_range_counts$Final_name == FALSE),]


#### Protocol for more complex cases -------------------------------------------
# These are species that  mostly occur inside their range but occasionally occur outside the range

# Decide whether to either:
# (a) remove the species from the entire database if the issue is found in >1 study, it's likely that the range map is outdated or there's an issue with taxonomy of that species
# (b) remove the species from just 1 study if it's an isolated issue, e.g. a suspected difference in classification or identification in just one study

## Subset the data for species that have occurrences both inside and >200km outside the range
# it's only 'problematic' if there are _presences_ >200km outside the range (absences outside the range are to be expected)



sometimes_outside_range <- st_drop_geometry(data) %>%
  filter(nearest_edge_dist_km < -200) %>%
  group_by(Final_name) %>%
  summarise(total_presences = sum(Occupancy)) %>%
  filter(total_presences >= 1) %>%
  inner_join(data, by = "Final_name")

inside_range <-  data %>%
  anti_join(sometimes_outside_range, by = "Final_name")

nrow(inside_range) + nrow(sometimes_outside_range)
nrow(data)


## Split by species to make a list of dataframes
sometimes_outside_range_split <- split(sometimes_outside_range, sometimes_outside_range$Final_name)


Deal_with_negatives <- function(species_data) { 
  
  # Initialize species_data_final as NULL
  species_data_final <- data.frame()
  
  # Identify the 'problems' - presences > 200km outside range
  species_data <- species_data %>% group_by(Study) %>% 
    mutate(problem = ifelse(nearest_edge_dist_km < -200 & Occupancy == 1, 'PROBLEM', 'fine'))
  
  
  
  ###### If >1 study has negative presences, remove the species -------
  
  ## Check if there is more than one study with a problem
  species_issue <- length(unique(species_data$Study[species_data$problem == "PROBLEM"])) > 1
  
  
  
  if (species_issue == TRUE) {  
    
    # - get the species name
    species_name <- unique(species_data$Final_name[species_data$problem == "PROBLEM"])
    
    # - print the message with the species name
    print(paste("Remove", species_name, "as there are presences >200km outside range in more than one study"))
    
    # Remove that species
    species_data_final <- NULL
    
  } 
  
  ###### If only 1 study has negative presence(s), just remove that study for this species -----------
  
  
  study_issue <- length(unique(species_data$Study[species_data$problem == "PROBLEM"])) == 1
  
  
  if (study_issue == TRUE) {  
    
    # Remove that one study for this species
    species_data_final <- subset(species_data, !(Study %in% species_data$Study[species_data$problem == "PROBLEM"]))
    
    
  } 
  
  
  return(species_data_final)
  
}

# - apply the function over all the species where there are any reports of presence >250km outside range
Dealt_with <- lapply(sometimes_outside_range_split, Deal_with_negatives)

# - get rid of the species with taxonomic issues
Dealt_with <- Dealt_with[sapply(Dealt_with, class) != "NULL"]

# - put the data together
Dealt_with_data <- as.data.frame(do.call(rbind, Dealt_with))

### Checking
hist(Dealt_with_data$nearest_edge_dist_km)
summary(Dealt_with_data$nearest_edge_dist_km)
summary(data$nearest_edge_dist_km)
negatives <- subset(Dealt_with_data, nearest_edge_dist_km < -200)

summary(negatives$nearest_edge_dist_km)
summary(negatives$Occupancy)



## Save data
colnames(Dealt_with_data)
Dealt_with_data <- subset(Dealt_with_data, select = - c(problem, total_presences, X, nearest_edge_dist_km))

colnames(inside_range)
inside_range <- subset(inside_range, select = c("Database","Study","SiteID","Final_name",
                                                "Occupancy","Method","Year","Realm",
                                                "main_forest_type","nearest_edge_dist","nearest_non_coastal_edge_dist",
                                                "Site_Longitude_x_behr","Site_Latitude_y_behr","nearest_non_coastal_edge_dist_km"))

colnames(Dealt_with_data)
Dealt_with_data <- subset(Dealt_with_data, select = c("Database","Study","SiteID","Final_name",
                                                "Occupancy","Method","Year","Realm",
                                                "main_forest_type","nearest_edge_dist","nearest_non_coastal_edge_dist",
                                                "Site_Longitude_x_behr","Site_Latitude_y_behr","nearest_non_coastal_edge_dist_km"))

data_final <- as.data.frame(rbind(inside_range, Dealt_with_data))


length(unique(data_final$Study))
length(unique(data_final$Final_name))



write.csv(data_final, 'data/processed/PresAbs/CLEANED_FOREST_COMBINED_PresAbs_Americas.csv')
