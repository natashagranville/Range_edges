#### PREDICTS data for birds across the Americas -----------

# remotes::install_github("timnewbold/predicts-demo",subdir="predictsFunctions")

library(dplyr)
library(predictsFunctions)
library(sf)

#### Get data ------------------------------------------------------------------
predicts <- readRDS(url("https://timnewbold.github.io/predicts_database.rds?dl=1"))

#### Pre-processing ------------------------------------------------------------
# https://timnewbold.github.io/PredictsIntroduction.html

# Correct effort-sensitive abundance measures (assumes linear relationship between effort and recorded abundance)
predicts <- predictsFunctions::CorrectSamplingEffort(diversity = predicts)

# Combine sites with identical coordinates, belonging to same study and spatial block of sites, sampled on same dates with same method and metric
predicts <- predictsFunctions::MergeSites(diversity = predicts,silent = TRUE)

#### Subset for American birds -------------------------------------------------

## Birds
birds <- subset(predicts, Class == 'Aves')


## Subset for Nearctic and Neotropical realms
data <- subset(birds, Realm == "Nearctic" | Realm == "Neotropic")

length(unique(data$Diversity_metric_is_effort_sensitive))
length(unique(data$Study_name))
length(unique(data$Best_guess_binomial))

#### Remove missing coords -----------------------------------------------------

## remove the missing coordinates
data <- subset(data, !is.na(data$Longitude | data$Latitude))

#### Species names -------------------------------------------------------------

# - remove rows where genus and species are not listed (e.g. unidentified species)
data <- subset(data, Genus != "")
data <- subset(data, Species != "")

# - Join genus and species to give scientific name
data$Scientific_name <-  paste(data$Genus, data$Species, sep = '_')

# (Check that Genus_species matches the Best_guess_binomial)
data$Best_guess_binomial <- gsub(' ', '_', data$Best_guess_binomial)
sum(data$Scientific_name == data$Best_guess_binomial)
sum(data$Scientific_name == data$Best_guess_binomial) == nrow(data)


#### Convert abundance to occurrence -------------------------------------------

# - remove the (relatively few) measurements that are not abundance or occurrence
data <- subset(data, Diversity_metric == 'abundance' | Diversity_metric == 'occurrence')

# - convert abundance to occurrence
data$occ <- ifelse(data$Measurement>0,1,0)

#### Sampling year(s) -----------------------------------------------------------

# - extract just the years from the start and end dates
data$Start_Year <- substr(data$Sample_start_earliest, 1, 4)
data$End_Year <- substr(data$Sample_end_latest, 1, 4)

# - combine start year and end year into one column
data$Year <- NA
  for (i in 1:nrow(data)) {
  if (data$Start_Year[i] == data$End_Year[i]){
    data$Year[i] <- data$Start_Year[i] 
  } else {
    data$Year[i] <- paste(data$Start_Year[i], data$End_Year[i], sep = '-')
  }
}



#### Study names ---------------------------------------------------------------

## make the Study column
# re-format the reference column to be consistent with the data from other databases
# so the study name is just first author and year

data$Study <- paste(gsub("^([A-Za-z]+).*", "\\1", data$Reference), 
                        as.integer(gsub("^.*([0-9]{4}).*", "\\1", data$Reference)), 
                        sep = '')

## check the names and correct if needed
unique(data$Study)
data$Study[data$Study == "B2010"] <- "Bóçon2010"
data$Study[data$Study == "O2007"] <- "O'Dea2007"
data$Study[data$Study == "St2007"] <- "St-Laurent2007"


#### Select continental studies only -------------------------------------------
data <- subset(data, Study != 'Suarez2009')


## Also remove Bóçon2010 where all the sites have the same coordinate
data <- subset(data, Study != 'Bóçon2010')


#### Site ID -------------------------------------------------------------------
## make a SiteID to uniquely identify each site _in each study_
data$SiteID <- paste(data$Study, data$Site_number, sep = '_')



#### Select columns, re-format and write.csv ----------------------------------------------

# label database
data$Database <- 'PREDICTS'


# select just columns of interest
colnames(data)
data <- subset(data, select = c("Database", "Study", "SiteID", 
                                        "Scientific_name", "occ",
                                        "Latitude", "Longitude",
                                        "Sampling_method", "Year", "Realm"))

# rename columns to match with the other data that I'm putting in the database
names(data)[names(data) == 'occ'] <- 'Occupancy'
names(data)[names(data) == 'Sampling_method'] <- 'Method'
names(data)[names(data) == 'Scientific_name'] <- 'Species'

# re-format the Methods to match with the other data that I'm putting in the database
data$Method <- as.character(data$Method)
unique(data$Method)

data$Method[data$Method == "mist-netting"] <- "Mist_net"
data$Method[data$Method == "point counts"] <- "Point_count"
data$Method[data$Method == "line/belt transects"] <- "Line_transect"
data$Method[data$Method == "systematic searching"] <- "Systematic_searching"
data$Method[data$Method == "visual encounter survey"] <- "Visual_encounter_survey"

data$Realm <- as.character(data$Realm)

data$Realm[data$Realm == "Neotropic"] <- "Neotropical"


write.csv(data, 'data/processed/PresAbs/PREDICTS_Americas_PresAbs_Birds_NOT_BirdLife_Aligned.csv')


