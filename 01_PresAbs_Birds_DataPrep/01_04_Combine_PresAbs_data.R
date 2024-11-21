#### Combining data ------------------------------------------------------------
library(dplyr)
library(sf)

ATLANTIC <- read.csv('data/processed/PresAbs/ATLANTIC_PresAbs_NOT_BirdLife_Aligned.csv')
BIOFRAG <- read.csv('data/processed/PresAbs/BIOFRAG_Americas_PresAbs_Birds_NOT_BirdLife_Aligned.csv')
PREDICTS <- read.csv('data/processed/PresAbs/PREDICTS_Americas_PresAbs_Birds_NOT_BirdLife_Aligned.csv')

#### Convert to spatial dataframes, combine and ensure consistent CRS ----------
ATLANTIC <- st_as_sf(ATLANTIC, coords = c('Longitude','Latitude'), crs = 4326) # WGS84
BIOFRAG <- st_as_sf(BIOFRAG, coords = c('Longitude','Latitude'), crs = 4326) # WGS84
PREDICTS <- st_as_sf(PREDICTS, coords = c('Longitude','Latitude'), crs = 4326) # WGS84


names(PREDICTS)[names(PREDICTS) == 'Scientific_name'] <- 'Species'
BIOFRAG <- subset(BIOFRAG, select = -Site)

## label realm
ATLANTIC$Realm <- 'Neotropical' 

### Combine everything together
DATA <- rbind(ATLANTIC, BIOFRAG, PREDICTS)

length(unique(ATLANTIC$Study))
length(unique(ATLANTIC$Species))

length(unique(BIOFRAG$Study))
length(unique(BIOFRAG$Species))

length(unique(PREDICTS$Study))
length(unique(PREDICTS$Species))

length(unique(DATA$Study))
length(unique(DATA$Species))


#### Remove duplicate studies -------------------------------------------------
# studies that are in >1 database

# get study author names
ATLANTIC_studies <- unique(subset(DATA, Database == "ATLANTIC")$Study)
BIOFRAG_studies <- unique(subset(DATA, Database == "BIOFRAG")$Study)
PREDICTS_studies <- unique(subset(DATA, Database == "PREDICTS")$Study)

# IMPORTANT - manually check whether any names match 
sort(ATLANTIC_studies)
sort(BIOFRAG_studies)
sort(PREDICTS_studies)

## remove duplicates (keep the biofrag versions of duplicate studies)
DATA <- subset(DATA,! Study %in% c("Banks-Leite2009", "Stouffer2011", "Barlow2007", "Lasky2010", "Develey2004", "Martensen2008"))


## How many studies and species
length(unique(DATA$Study))
length(unique(DATA$Species))


#### Filter years --------------------------------------------------------------

# IMPORTANT - manually check and filter the years

## look at the years the data were collected
years <- st_drop_geometry(DATA) %>% group_by(Year) %>% summarise(n())
print(years, n = nrow(years))

## restrict the dataset to studies conducted after or during 2000 (since that's when the Hansen forest cover data is from)
# so include any studies done after 2000, as well as studies that spanned the year 2000
DATA <- subset(DATA, ! Year %in% c("", "1977-1978", "1978", "1988-1991", "1991","1991-1992",
                                               "1992-1993", "1992-1994", "1994", "1994-1995", "1996",
                                               "1996-1998", "1997","1997-1998", "1998"))


length(unique(DATA$Study))
length(unique(DATA$Species))



#### Align taxonomy ------------------------------------------------------------------
# 
# ## Species names on database
# Data_species <- as.data.frame(sort(unique(DATA$Species)))
# colnames(Data_species) <- 'Scientific_name'
# 
# ## BirdLife names
# BirdLife_species <- read.csv("Taxonomy/BirdLife_species_list.csv")
# BirdLife_species <- as.data.frame(sort(unique(BirdLife_species$Scientific.name)))
# colnames(BirdLife_species) <- 'Scientific_name'
# BirdLife_species$Scientific_name <- gsub( ' ', '_', BirdLife_species$Scientific_name)
# 
# ## Find matching species names
# match_id <- match(Data_species$Scientific_name, BirdLife_species$Scientific_name)
# 
# ## Create merged data frame with matching species
# species_data <- data.frame(Data_species = Data_species$Scientific_name,
#                            BirdLife_species = ifelse(is.na(match_id), NA, BirdLife_species$Scientific_name[match_id]))
# 
# ## Add the database
# species_data$source_database <- sapply(species_data$Data_species, function(Data_species) {
#      PREDICTS_df <- ifelse(Data_species %in% PREDICTS$Species, 'PREDICTS', '')
#      ATLANTIC_df <- ifelse(Data_species %in% ATLANTIC$Species, 'ATLANTIC', '')
#      BIOFRAG_df <- ifelse(Data_species %in% BIOFRAG$Species, 'BIOFRAG', '')
#     return(paste(PREDICTS_df, ATLANTIC_df, BIOFRAG_df))
#   })
# 
# 
# ## Add total number of observations
# species_counts <- DATA %>% group_by(Species) %>% summarise(total_presences = sum(Occupancy))
# 
# species_data$total_presences <- species_counts$total_presences
# 
# ## Remove species with zero total presences
# species_data <- subset(species_data, total_presences > 0)
# 
# ## Put columns in desired order
# species_data <- species_data[, c('source_database', 'total_presences', 'Data_species', 'BirdLife_species')]
# 
# write.csv(species_data, 'taxonomy/species_data.csv')
# 
# # ## I went through species-by-species according to the following protocol:
# # (1) if the dataset and BirdLife have the same name, it's all good so do nothing  (this was the large majority of cases)
# # (2) if the species name is in the dataset but not in BirdLife, search (mainly in the BirdLife handbook version 8) for synonyms or re-classifications and suggest an alternative name if appropriate
# # (2.1) then if there are presence observations greater than about 200 km _outside_ the range of that alternatively-named species, reject the alternative name and remove the species due to taxonomic issues
# # (3) if a species has zero total observations, remove it.
# # (4) remove taxa that were not identified to species level
# 


## Read in the csv file with the current updated final species list
final_species_list <- read.csv("taxonomy/species_data_annotated.csv")

## Merge the main dataframe with the final species list  
# based on the _old_ species names which are found in both dataframes
colnames(final_species_list)[colnames(final_species_list) == 'Data_species'] <- 'Species'

DATA_with_final_species <- left_join(DATA, final_species_list,  by = 'Species')

## Remove rows where there are blanks in the final_name column
# these are the species that we're removing due to taxonomic issues
DATA_FINAL <- DATA_with_final_species[-which(DATA_with_final_species$Final_name == ""), ]

## Make the final_name column the _only_ species name column
colnames(DATA_FINAL)
DATA_FINAL <- subset(DATA_FINAL, select = c("Database", "Study", "SiteID", 
                                "Final_name", "Occupancy", "Method", "Year", "Realm", "geometry"))


## Ensure there are underscores between the genus and species for every name
DATA_FINAL$Final_name <- trimws(DATA_FINAL$Final_name)
DATA_FINAL$Final_name <- gsub(' ', '_', DATA_FINAL$Final_name)

# (How many species have we got now?)
length(unique(DATA_FINAL$Study))
length(unique(DATA_FINAL$Final_name))




#### Save data -----------------------------------------------------------------
coords <- st_coordinates(DATA_FINAL)
DATA_FINAL$Site_Longitude_x <- coords[,1]
DATA_FINAL$Site_Latitude_y <- coords[,2]

write.csv(st_drop_geometry(DATA_FINAL), "data/processed/PresAbs/COMBINED_PresAbs_Americas.csv")

