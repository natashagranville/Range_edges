#### Bird incidence data from Atlantic Bird data paper in Ecology -------------------------------------------
# source:  https://doi.org/10.1002/ecy.2119


library(dplyr)
library(stringr)

raw_data <- read.csv("data/raw/Atlantic_birds_Ecology_data/ATLANTIC_BIRDS_quantitative.csv")
refs <- read.csv("data/raw/Atlantic_birds_Ecology_data/ATLANTIC_BIRDS_refs.csv")

#### Transform relative abundance data to presence-absence data on a per-study basis ----------

## Make a column for occurrence and put 1 for all the present species
raw_data$occ <- 1

## Split up into studies
# - Get study references
raw_data <- left_join(raw_data, refs[c('Bib_Ref', 'Site', 'ID_codref')], by = 'ID_codref')
raw_data$Bib_Ref <- as.factor(raw_data$Bib_Ref)

# - Give each study a number to make indexing easier
raw_data$study_number <- as.numeric(factor(raw_data$Bib_Ref, levels = unique(raw_data$Bib_Ref)))

length(unique(raw_data$Species))
length(unique(raw_data$study_number))

# - Split up data by study
split_by_study <- split(raw_data, raw_data$study_number)

## Function wrapper to transform relative abundance to presence absence FOR EACH STUDY SEPARATELY: 

Transform_RelAbund_to_PresAbs <- function(data) {
    
  ## get a list of all species
  species <- unique(data$Species)
  
  ## Combine lat and long into one column to uniquely identify site locations
  data$site <- paste('lat_', data$Latitude_y, '_lon_',  data$Longitude_x, sep = '')
  
  ## give each study a number to make indexing easier
  data$site_number <- as.numeric(factor(data$site, levels = unique(data$site)))
  
  
  ## select just columns of interest to make it a bit easier to work with 
  data <- data[c('Record_id', 'ID_codref', 'Methods', 'site', 'site_number',
                 "Latitude_y", "Longitude_x",
                  'Species', 'Year_start', 'Year_finish', 'occ')]
  
  ## remove duplicates
  duplicates <- duplicated(data[c('Methods', 'site', 'site_number',
                                        'Species', 'Year_start', 'Year_finish', 'occ')])
  
  data <- data[!duplicates,]
  

  
  
  
  # !! only continue if there is >1 site !!
  if(length(unique(data$site)) > 1){
    
    
    # - split up data by study
    split_by_site <- split(data, data$site_number)
    
    
    # loop over each site
    all_sites <- data.frame()
    
    # site <- split_by_site[[1]]
    for(i in 1:length(split_by_site)){
      
      site <- split_by_site[[i]]
      
      ### Put 0 for any species that are not at this site (but are in this study) 
      
      # - find absent species at this site
      absent_species <- species[! species %in% site$Species]
      
      if(length(absent_species) > 0) {
        
       
        # - make dataframe of absent species
        other_cols <- site[,c("Record_id", "ID_codref", "Methods",
                                           "Latitude_y", "Longitude_x",  "site", "site_number",
                                           "Year_start", "Year_finish")]
        
        other_cols_1row <- other_cols[1,]
  
        other_cols_CorrectLength <- other_cols_1row %>% slice(rep(1:length(absent_species), each = length(absent_species)))
        
        
        absent_data <- cbind(other_cols_CorrectLength, absent_species)
        absent_data$occ <- 0
        
        
        # - rename and reorder columns
        names(absent_data)[names(absent_data) == 'absent_species'] <- 'Species'
        absent_data <- absent_data[, colnames(site)]
        
        # - stick onto the dataframe of present species
        site <- rbind(site, absent_data)
        
      }
      
      # then combine that with the main dataframe to combine all sites together
      all_sites <- rbind(all_sites, site)
      
      
      
    }
    
    return(all_sites)
    
    
  } else {
    
    return('Only one site in study')

  }
  
}

## Apply the function over each study
transformed <- lapply(split_by_study, Transform_RelAbund_to_PresAbs)


## Now string that together into a dataframe

# - get rid of studies that only had one site
Atlantic_data_list <- transformed[sapply(transformed, class) != "character"]

# see how many studies were removed
removed_studies <- transformed[sapply(transformed, class) == "character"]
removed_data <- raw_data[raw_data$study_number %in% as.numeric(names(removed_studies)),]
removed_refs <- as.data.frame(unique(removed_data$Bib_Ref))

# - put the data together
Atlantic_data <- as.data.frame(do.call(rbind, Atlantic_data_list))

length(unique(Atlantic_data$Species))
length(unique(Atlantic_data$study_number))

#### Study citations ------------------------------------------------------------
# - get study info
Atlantic_data <- left_join(Atlantic_data, raw_data[,c('Bib_Ref', 'Record_id')], by = 'Record_id')

# - split at first comma to get author name
Atlantic_data$Author <- str_split_fixed(Atlantic_data$Bib_Ref, pattern = ',', n = 2)[,1]

# - extract the year using a regular expression 
Atlantic_data$Citation_Year <- str_extract(Atlantic_data$Bib_Ref, "\\b\\d{4}\\b") # \\b\\d{4}\\b is used to match a sequence of four digits surrounded by word boundaries

# - join year and author to make a study citation column
Atlantic_data$Citation <- paste(Atlantic_data$Author, Atlantic_data$Citation_Year, sep = '')

# - remove year and author columns
Atlantic_data <- Atlantic_data[,!names(Atlantic_data) %in% c('Author', 'Citation_Year')]


## check the names and correct if needed
print(unique(Atlantic_data$Citation))

Atlantic_data$Citation[Atlantic_data$Citation == "Banks2009"] <- "Banks-Leite2009"
Atlantic_data$Citation[Atlantic_data$Citation == "D\xe1rio1999"] <- "Dário1999"
Atlantic_data$Citation[Atlantic_data$Citation == "D\xe1rio2002"] <- "Dário2002"
Atlantic_data$Citation[Atlantic_data$Citation == "Lobo-Ara\xfajo2013"] <- "Lobo-Araújo2013"
Atlantic_data$Citation[Atlantic_data$Citation == "Athi\xea2009"] <- "Athiê2009"

print(unique(Atlantic_data$Citation))

## check the years and correct if needed
Atlantic_data$Citation[Atlantic_data$Citation == "LuzNA"] <- "Luz2012"
Atlantic_data$Citation[Atlantic_data$Citation == "DeveleyNA"] <- "Develey2004"
Atlantic_data$Citation[Atlantic_data$Citation == "PozzaNA"] <- "Pozza2002"
Atlantic_data$Citation[Atlantic_data$Citation == "AlmeidaNA"] <- "Almeida1997"
Atlantic_data$Citation[Atlantic_data$Citation == "GussoniNA"] <- "Gussoni2007"


print(unique(Atlantic_data$Citation))

# correct Longo_2007 year start
Atlantic_data[Atlantic_data$Citation %in% 'Longo2007',]$Year_start <- 2005

# - remove Bib_Ref column 
Atlantic_data <- Atlantic_data[,!names(Atlantic_data) %in% 'Bib_Ref']

length(unique(Atlantic_data$Citation))
length(unique(Atlantic_data$Species))

# note - sometimes the years are the year of publication rather than data collection
# I'm only putting the years in the citations to differentiate between studies conducted by the same author
# The actual sampling years are in a separate column

#### Sampling year(s) ----------------------------------------------------------

Atlantic_data$Year <- NA

# combine start year and end year into one column
for (i in 1:nrow(Atlantic_data)) {
  if (Atlantic_data$Year_start[i] == Atlantic_data$Year_finish[i]){
    Atlantic_data$Year[i] <- Atlantic_data$Year_start[i] 
  } else {
    Atlantic_data$Year[i] <- paste(Atlantic_data$Year_start[i], Atlantic_data$Year_finish[i], sep = '-')
  }
}

#### Site ID -------------------------------------------------------------------
## make a site ID to uniquely identify each site _in each study_
Atlantic_data$SiteID <- paste(Atlantic_data$Citation, Atlantic_data$site_number, sep = '_')

unique(Atlantic_data$SiteID)

#### Select columns, re-format and write.csv ----------------------------------------------
Atlantic_data$Database <- 'ATLANTIC'

Atlantic_data$Species <- gsub(' ', '_', Atlantic_data$Species)

# select just columns of interest
colnames(Atlantic_data)
Atlantic_data <- subset(Atlantic_data, select = c("Database", "Citation", "SiteID", 
                                "Species", "occ",
                                "Latitude_y",  "Longitude_x",
                                "Methods", "Year"))

# rename columns to match with the other data that I'm putting in the database
names(Atlantic_data)[names(Atlantic_data) == 'Citation'] <- 'Study'
names(Atlantic_data)[names(Atlantic_data) == 'occ'] <- 'Occupancy'
names(Atlantic_data)[names(Atlantic_data) == 'Latitude_y'] <- 'Latitude'
names(Atlantic_data)[names(Atlantic_data) == 'Longitude_x'] <- 'Longitude'
names(Atlantic_data)[names(Atlantic_data) == 'Methods'] <- 'Method'

# re-format the Methods to match with the other data that I'm putting in the database
Atlantic_data$Method[Atlantic_data$Method == "Mist net"] <- "Mist_net"
Atlantic_data$Method[Atlantic_data$Method == "Point counts"] <- "Point_count"
Atlantic_data$Method[Atlantic_data$Method == "Line transect"] <- "Line_transect"


length(unique(Atlantic_data$Study))
length(unique(Atlantic_data$Species))

## save data
write.csv(Atlantic_data, 'data/processed/PresAbs/ATLANTIC_PresAbs_NOT_BirdLife_Aligned.csv')



