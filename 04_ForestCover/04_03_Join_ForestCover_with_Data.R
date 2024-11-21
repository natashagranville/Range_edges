##### Join all data together - distance to range edge and forest cover  ---------

library(dplyr)
library(readr)
library(sf)


##### Read in data -------------------------------------------------------------

## Distances to different types of range edges
setwd("data/processed/HardVsSoft_PolewardVsEquatorward_data")
data <- data.frame()
data_species <- data.frame()
for (file in list.files(pattern = ".csv")){
  data_species <- read_csv(file)
  data <- rbind(data, data_species)
}
setwd("C:/Users/tasha/OneDrive - Imperial College London/PhD/PROJECT_RangeEdgeType")

## Forest cover
fp70 <- read.csv('data/processed/Forest_cover_proportions_Hansen/Forest70_proportions_600m.csv')

## Join
data <- left_join(data, fp70, by = 'SiteID')



#### Transformations of distances  -----------------------
data$inland_edge_dists_km <- data$inland_edge_dists / 1000
data$coastal_edge_dists_km <- data$coastal_edge_dists / 1000

data$sqrt_inland_edge_dists_km <- sign(data$inland_edge_dists_km) * sqrt(abs(data$inland_edge_dists_km))
data$sqrt_coastal_edge_dists_km <- sign(data$coastal_edge_dists_km) * sqrt(abs(data$coastal_edge_dists_km))

#### Remove unnecessary columns --------------
colnames(data)
data <- data %>% dplyr::select( - c("...1", "X.x", "X.y"))


#### Basic info ------------------------

# How many studies were done with point counts, how many by mist nets etc?
methods_summary <- data %>% group_by(Study, Method) %>% summarise(n())

methods_summary_summary <- methods_summary %>% group_by(Method) %>% summarise(n())
methods_summary_summary

# How many sites, studies and species?
length(unique(data$Study))
length(unique(data$Final_name))
length(unique(data$SiteID))

unique(data$Year)


unique(data$Method)

split <- split(data, data$Study)
unique(split[[18]]$Method)
split[[18]] %>% group_by(Method) %>% summarise(n())


# how many species observed outside their range
negatives <- subset(data, sqrt_inland_edge_dists_km < 0)
negative_presences <- subset(negatives, Occupancy == 1)
total_presences <- subset(data, Occupancy == 1)
nrow(negative_presences)
nrow(total_presences)

nrow(negative_presences) / nrow(total_presences) * 100


## How many species sampled in >1 study
study_counts <- data %>% group_by(Final_name) %>% summarise(count = length(unique(Study)))
sum(study_counts$count > 1)

##### Save data ------------------------------------------
write.csv(as.data.frame(data), "data/processed/Final_data.csv")
