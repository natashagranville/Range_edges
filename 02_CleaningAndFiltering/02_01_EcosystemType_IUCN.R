#### Classify studies by IUCN ecosystem typology --------------
# https://zenodo.org/records/10081251
# https://www.nature.com/articles/s41586-022-05318-4

library(dplyr)
library(ggplot2)
library(sf)
library(stringr)
library(terra)


## Get list of ecosystem types and corresponding codes
# https://static-content.springer.com/esm/art%3A10.1038%2Fs41586-022-05318-4/MediaObjects/41586_2022_5318_MOESM7_ESM.pdf
ecosystem_types_list <- read.csv("data/processed/IUCN_terrestrial_ecosystem_types_list.csv")

##### Get all terrestrial ecosystem type rasters ----------------------------

# - all ecosystem type files
files <- list.files("data/raw/IUCN_ecosystem_typology/all-maps-raster-geotiff/")

# - select just terrestrial
terrestrial_files <- files[grep('T', files)]

# - get full file paths
terrestrial_files_list <- lapply(terrestrial_files, function(x) paste("data/raw/IUCN_ecosystem_typology/all-maps-raster-geotiff/", x, sep = ''))

# - get list of rasters
raster_list <- lapply(terrestrial_files_list, rast)


## Ensure consistent crs
# - get crs
crs_list <- lapply(raster_list, function(x) crs(x))
unique(crs_list)
raster_list[[13]]
# note that T1.3 (tropical/subtropical montane forest) has a different crs

# - reproject this one raster so they're all in the same crs
raster_list[[13]] <- project(raster_list[[13]], raster_list[[1]])

# - now check crs again
crs_list <- lapply(raster_list, function(x) crs(x))
unique(crs_list)
rasters_crs <- unique(crs_list)


## Stack rasters 
raster_stack <- rast(raster_list)
nlyr(raster_stack)

## Plot rasters
plot(raster_stack)

# Have a look at the forest rasters
plot(raster_stack$T1.1.IM.mix_v2.0)
plot(raster_stack$T1.2.web.mix_v1.0)
plot(raster_stack$T1.3.WM.nwx_v1.0)
plot(raster_stack$T1.4.web.orig_v2.0)
plot(raster_stack$T2.1.web.mix_v1.0)
plot(raster_stack$T2.2.web.mix_v1.0)
plot(raster_stack$T2.3.web.orig_v1.0)
plot(raster_stack$T2.4.web.orig_v2.0)
plot(raster_stack$T2.5.web.alt_v2.0)
plot(raster_stack$T2.6.web.mix_v1.0)



#####  Get spatial data for each study -----------------------------------------------------------
###### Get data -----------------------
data <- read.csv("data/processed/PresAbs/COMBINED_PresAbs_Americas.csv")

# - convert to spatial feature dataset
data <- st_as_sf(data, coords = c('Site_Longitude_x', 'Site_Latitude_y'), crs = 4326)

st_crs(data) == crs(raster_stack)

plot(raster_stack[[1]])
plot(st_geometry(data), add = T)

##### Group by study and get convex hull around study area ---------------

## Get convex hull around all sites within each study
studies <- data %>%
  group_by(Study) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_convex_hull() %>%
  left_join(data %>%
              distinct(Study, Database),
            by = "Study")


study <- studies[59,]

##### Extract ecosystem type data  -----------------------------------------------

studies_with_forest_types <- data.frame()
studies_with_non_forest_types <- data.frame()
#non_forest_main_habitat_types <- NA

for (i in 1:nrow(studies)) {
  
  study <- studies[i,]
  
  
  ####### Get habitat types  -----------------------------------------------------
  
  # - extract values of all raster layers (habitat types)
  extracted <- terra::extract(raster_stack, study)
  
  # - select columns where there are 1s or 2s (representing major and minor habitat types respectively)
  habitat_types <-  Filter(function(x) any(x == 1 | x == 2), extracted)
  # NOTE - each row here is a raster cell, _not_ a site
  
  # - that's the file name. now isolate the ecosystem type code
  habitat_type_codes_split <- strsplit(colnames(habitat_types), '\\.')
  habitat_type_codes <-  lapply(habitat_type_codes_split, function(x) paste(x[1], x[2], sep = "."))
  colnames(habitat_types) <- habitat_type_codes
  
  ###### Remove non-forest studies ----------------------------------------------
  ## Remove studies where there is absolutely no forest in the study area
  forest_codes <- c('T1.1', 'T1.2', 'T1.3', 'T1.4',
                    'T2.1', 'T2.2', 'T2.3', 'T2.4', 'T2.5', 'T2.6' )
  
 
  if(! any(colnames(habitat_types) %in% forest_codes))  { 
    
    ## find out what the main habitat type is anyway
    
    habitat_types <- habitat_types %>% mutate_all(~ifelse(. == 1, 4, .)) %>% subset(select = - ID.NA)
    
    colSums(habitat_types, na.rm = TRUE)
    
    # select the main forest type
    study$non_forest_main_habitat_type <- names(which.max(colSums(habitat_types, na.rm = TRUE)))
    
    print(paste("Removing study", unique(study$Study), "as it's not in any forest habitat", " Main habitat type is:", study$non_forest_main_habitat_type))
    
    studies_with_non_forest_types <- rbind(studies_with_non_forest_types, study)
    
    
  } else  {
    
  ###### Label forest studies with the main forest type -------------------------
    
    forest_habitat_types <- habitat_types[colnames(habitat_types) %in% forest_codes]
    
    
    
    # when the same study gets classified under multiple different types of forest ...
    
    # choose the one that occurs most frequently throughout the study region
    # but weight it by major vs minor
    # currently 1 is major and 2 is minor
    
    # I will convert all the 1s to 4s, then take the sum of the values and select the classification with the highest sum of values
    # that way 'major' gets a higher weighting than 'minor' (4 is now major and 2 is minor)
    forest_habitat_types <- forest_habitat_types %>% mutate_all(~ifelse(. == 1, 4, .))
    

    colSums(forest_habitat_types, na.rm = TRUE)
    
    # select the maim forest type
    study$main_forest_type <- names(which.max(colSums(forest_habitat_types, na.rm = TRUE)))
    
    studies_with_forest_types <- rbind(studies_with_forest_types, study)
    
    
  }
  
}

##### Look at how many studies there are in each forest type --------------------
summary <- st_drop_geometry(studies_with_forest_types) %>% 
  group_by(main_forest_type) %>% 
  summarise(number_of_studies = n())

summary

### and in each habitat type

summary_non_forest <- st_drop_geometry(studies_with_non_forest_types) %>% 
  group_by(non_forest_main_habitat_type) %>% 
  summarise(number_of_studies = n())
summary_non_forest

colnames(summary_non_forest) = c('Ecosystem_functional_group_code', 'number_of_studies')

summary_non_forest <- left_join(summary_non_forest, ecosystem_types_list, by = 'Ecosystem_functional_group_code')

summary_non_forest__by_biome <- st_drop_geometry(summary_non_forest) %>% 
  group_by(Biome_name) %>% 
  summarise(number_of_studies = sum(number_of_studies))

summary_non_forest__by_biome

##### Save data -----------------------------------------------------------------
## join with main dataframe
coords <- st_coordinates(data)
data$Site_Longitude_x <- coords[,1]
data$Site_Latitude_y <- coords[,2]
data <- st_drop_geometry(data)

data_with_forest_types <- left_join(data, st_drop_geometry(studies_with_forest_types)[, c('Study', 'main_forest_type')], by = 'Study')

# REMOVE NON-FOREST STUDIES (rows with NA in main_forest_type)
forest_data_with_forest_types <- data_with_forest_types[!is.na(data_with_forest_types$main_forest_type),]

length(unique(forest_data_with_forest_types$Study))
length(unique(forest_data_with_forest_types$Final_name))


## write csv
write.csv(forest_data_with_forest_types, 'data/processed/PresAbs/FOREST_COMBINED_PresAbs_Americas.csv')
