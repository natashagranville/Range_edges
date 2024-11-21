#### BIOFRAG data for birds across the Americas -----------
library(dplyr)
library(stringr)
library(sf)
library(tidyr)

## Get the basic info about each biofrag study from the biofrag website:
# https://biofrag.wordpress.com/metainfos/
biofrag_all_info <- read.csv("data/processed/PresAbs/BIOFRAG_study_info_list_with_additions.csv")
biofrag_all_info$Taxon_Name <- trimws(biofrag_all_info$Taxon_Name )
biofrag_birds_info <- subset(biofrag_all_info, Taxon_Name =='Bird')


### Get list of PIDs (project IDs)
Bird_PIDs <- unique(sapply(str_split(list.files("data/raw/BIOFRAG_birds"), "_"), "[[", 1))


#### Wrangle the data for each study -------------------------------------------

wrangle_data_biofrag <- function(PID_study){
  
  ## BIOFRAG data for this study (PID)
  plot_study <- read.csv(paste("data/raw/BIOFRAG_birds/", PID_study, "_Plot.csv", sep = ""))
  biofrag_study_matrix <- read.csv(paste("data/raw/BIOFRAG_birds/", PID_study, "_species_matrix.csv", sep = ""))
  
  ## Subset for this study
  biofrag_study_info <- subset(biofrag_birds_info, PID == PID_study)
  
  ## Long format rather than matrix
  data_study <- pivot_longer(biofrag_study_matrix, cols = 2:ncol(biofrag_study_matrix), names_to = 'Species', values_to = 'abund_or_occ')
  
  ## Convert abundance to occurrence
  data_study$Occupancy <- ifelse(data_study$abund_or_occ > 0,1,0)
  
  
  ## Convert NaN and NA to 0
  data_study$Occupancy <- ifelse(is.nan(data_study$Occupancy) | is.na(data_study$Occupancy),
                                 0, data_study$Occupancy)
  
  data_study <- subset(data_study, select = - c(abund_or_occ))
  
  ## Use underscores instead of dots in species names
  data_study$Species <- gsub('\\.', '_', data_study$Species)
  
  ## Add method 
  data_study$Method <- biofrag_study_info$MeasureTechnique
  
  ## Add year
  Year_min <- str_split(biofrag_study_info$Year_min, ' ')[[1]][1]
  Year_max <- str_split(biofrag_study_info$Year_max, ' ')[[1]][1]
  
  if(Year_min == Year_max) {
    data_study$Year <- Year_min
  } else if(Year_max > Year_min) {
    data_study$Year <- paste(Year_min, '-', Year_max, sep = '')
  }
  
  ## Add realm
  data_study$Realm <- biofrag_study_info$Realm
  
  ## Combine with the plot info for this study to get the lat and long
  data_study <- left_join(data_study, plot_study, by = 'Plot')
  
  ## SiteID instead of plot name
  # to match with the way I'm labelling sites in the rest of my data
  data_study$SiteID <- paste(data_study$PID, data_study$Plot, sep = '_')
  
  
  ## Label study (PID_surname)
  name <- separate_wider_delim(data = biofrag_study_info, 
                                             cols = Contactname, 
                                             delim = " ", 
                                             names = c("Firstname", "Lastname"))
  data_study$Study <- paste(data_study$PID, name$Lastname, sep = "_")
  
  
  ## Label database
  data_study$Database <- 'BIOFRAG'
  
  
  ## Change column names to match the rest of the database
  names(data_study)[names(data_study) == 'Plot'] <- 'Site'
  
  colnames(data_study)
  
  data_study <- subset(data_study, select = c("Database", "Site", "SiteID", "Study",
                                                    "Species", "Occupancy",
                                                    "Latitude",  "Longitude",
                                                    "Method", "Year", "Realm"))
  
  return(data_study)
}

data_reformatted_list <- lapply(Bird_PIDs, wrangle_data_biofrag)




## PID1003 - remove sites that we don't have coords for

# - Isolate just the PID1003 dataframe from the list
PID1003_data <- data_reformatted_list[[which(Bird_PIDs == "PID1003")]]

# - Find which rows are missing coords
NAs <- PID1003_data[which(is.na(PID1003_data$Latitude)),]

# - Remove those rows from the PID1003 dataframe within the list of dataframe
data_reformatted_list[[which(Bird_PIDs == "PID1003")]] <- subset(data_reformatted_list[[which(Bird_PIDs == "PID1003")]], ! Site %in% NAs$Site)

## Combine the list of dataframes together to make one big dataframe with all studies
data_reformatted <- as.data.frame(do.call(rbind, data_reformatted_list))





## Restrict to Neotropical and Nearctic realms
final_data <- subset(data_reformatted, Realm == "Nearctic" | Realm == "Neotropical")

str(final_data)
summary(final_data$Occupancy)

length(unique(final_data$Species))
length(unique(final_data$Study))

## The following studies are from data used in Orme et al 2019 NEE, not the BIOFRAG database itself
additional_studies <- subset(final_data, Study %in% c("PID2001_Hatfield", "PID2002_dosAnjos", "PID2003_Uezu", "PID2004_Develey"))
BIOFRAG_without_additional_studies <- subset(final_data, ! Study %in% c("PID2001_Hatfield", "PID2002_dosAnjos", "PID2003_Uezu", "PID2004_Develey"))


length(unique(BIOFRAG_without_additional_studies$Species))
length(unique(BIOFRAG_without_additional_studies$Study))

length(unique(additional_studies$Species))
length(unique(additional_studies$Study))

write.csv(final_data, "data/processed/PresAbs/BIOFRAG_Americas_PresAbs_Birds_NOT_BirdLife_Aligned.csv")
