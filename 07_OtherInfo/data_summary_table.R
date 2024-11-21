#### Data summary table for supp mat ---------------------

library(dplyr)
library(sf)
library(units)

data <- read.csv('data/processed/Final_data.csv')

behr <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

data <- st_as_sf(data, coords = c("Site_Longitude_x", "Site_Latitude_y"), crs = behr)

data <- st_transform(data, 4326)




data_summary <- st_drop_geometry(data) %>% 
  group_by(Database, Study, Year, Realm) %>% 
  summarise( no_sites = length(unique(SiteID)),
             no_species = length(unique(Final_name))) %>%
  arrange(Database)


data_summary$Author <- data_summary$Study
write.csv(data_summary, "results/summary_data_table.csv")
