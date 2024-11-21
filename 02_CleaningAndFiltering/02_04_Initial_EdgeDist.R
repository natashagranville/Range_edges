#### Distance to geographic range edge --------------------
# For the purpose of cleaning the data 
  
setwd("C:/Users/tasha/OneDrive - Imperial College London/PhD/PROJECT_RangeEdgeType")

library(dplyr)
library(sf)
library(units)

behr <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

#### Get data ------------------------------------------------------------------
##### Ranges -------------
ranges <- st_read("data/processed/Cleaned_BirdLife_ranges_Americas/Cleaned_BirdLife_ranges_Americas.shp")


##### Coast -----
buffered_coastline <- st_read("data/processed/buffered_coastline/buffered_coastline.shp")
st_crs(ranges) == st_crs(buffered_coastline)

plot(st_geometry(buffered_coastline))

##### Bird count data ----------
data <- read.csv("data/processed/PresAbs/FOREST_COMBINED_PresAbs_Americas.csv")
data <- st_as_sf(data, coords = c("Site_Longitude_x", "Site_Latitude_y"), crs = 4326)

ranges <- subset(ranges, sci_nam %in% data$Final_name)


## Select only species that we have ranges for
data <- subset(data, Final_name %in% ranges$sci_nam)

# Transform to Behrmann projection
data <- st_transform(data, behr)



species_list <- unique(ranges$sci_nam)

for (i in 1:length(species_list)){
  
  tryCatch({
    
    species <- species_list[i]
    
    species_range <- subset(ranges, sci_nam == species)
    species_range_outline <- st_cast(species_range, 'MULTILINESTRING', do_split = FALSE)
    inland_edges <- st_intersection(species_range_outline, buffered_coastline)
    coastal_edges <- st_difference(species_range_outline, buffered_coastline)
    
    plot(st_geometry(species_range), col = 'lightblue1', axes = T)
    plot(st_geometry(inland_edges),  col = 'gold', add = T)
    plot(st_geometry(coastal_edges),  col = 'navyblue', add = T)
    
    species_data <- subset(data, Final_name == species)
    #plot(st_geometry(species_data), add = T)
    
    
    
    #### Calculate distance to range edge ------------------------------------------
    inland_distances <- data.frame(matrix(nrow = nrow(species_data), ncol = 1))
    colnames(inland_distances) <- 'inland_dist'
    
    inland_distances$inland_dist <- drop_units(st_distance(species_data, inland_edges))

    if (length(inland_distances$inland_dist) > 0) {
      
      in_range <- drop_units(st_distance(species_data, species_range)) # Check if species is in the range
      inland_distances$inland_dist <- inland_distances$inland_dist * ((in_range == 0)* 2 - 1) # Negative distances for sites outside range
      inland_distances <- as.data.frame(inland_distances)
      colnames(inland_distances) <- 'inland_dist'
      
    }

    
    coastal_distances <- data.frame(matrix(nrow = nrow(species_data), ncol = 1))
    colnames(coastal_distances) <- 'coastal_dist'
    
    
    coastal_distances$coastal_dist <- drop_units(st_distance(species_data, coastal_edges))
    coastal_distances <- as.data.frame(coastal_distances)
    

    
    ##### Distance to nearest edge and distance to nearest non-coastal edge -----
   
    species_data$nearest_edge_dist <- NA
    species_data$nearest_non_coastal_edge_dist <- NA
    
    # There are four mutually exclusive scenarios that need accounting for: 
    
    ## (1) If nearest edge is the coast
    # nearest edge and nearest non-coastal edge will be different
    
    ## (2) If nearest edge is not the coast 
    # so nearest edge and nearest non-coastal edge will be the same
    
    ## (3) If there are no coastal edges (species range is entirely inland)
    # so nearest edge and nearest non-coastal edge will be the same
    # won't be able to tell a difference between this and (2) without looking at the ranges, but either way the coast is not likely to be particularly relevant for this species at this site
    
    ## (4) If there are only coastal edges (e.g. island endemics)
    # nearest non-coastal edge will be NA because there are no non-coastal edges
    
    
    ###### If there are both inland edges and coastal edges --------------------
    
    if(nrow(inland_edges) == 1 & 
      nrow(coastal_edges) == 1 &
      identical(st_geometry(species_range_outline), st_geometry(inland_edges)) == FALSE &
      identical(st_geometry(species_range_outline), st_geometry(coastal_edges)) == FALSE) {
      
      

      for(i in 1:nrow(species_data)){
        
        if(inland_distances[i,] > coastal_distances[i,]) {
          
          ####### This is scenario (1) #######
          # nearest edge is the coast, so nearest edge and nearest non-coastal edge will be different
        
          species_data$nearest_edge_dist[i] <- coastal_distances[i,]
          species_data$nearest_non_coastal_edge_dist[i] <- inland_distances[i,]
        
        } else if (inland_distances[i,] < coastal_distances[i,]) {
        
          ####### This is scenario (2) #######
          # nearest edge is not the coast, so nearest edge and nearest non-coastal edge will be the same
          
          species_data$nearest_edge_dist[i] <- inland_distances[i,]
          species_data$nearest_non_coastal_edge_dist[i] <- inland_distances[i,]
          
        }
      }
      
      }
   
      ####### If there are no coastal edges (entire range is inland) --------------
      if(nrow(inland_edges) == 1 & 
       nrow(coastal_edges) == 0) {
      
          ####### This is scenario (3) #######
          species_data$nearest_edge_dist <- inland_distances$inland_dist
          species_data$nearest_non_coastal_edge_dist <- inland_distances$inland_dist
    
      }
    
    ####### If there are only coastal edges (e.g. island endemics) --------------
    if(nrow(inland_edges) == 0 & 
       nrow(coastal_edges) == 1) {
      
        ####### This is scenario (4) #######
        species_data$nearest_edge_dist <- coastal_distances$coastal_dist
        species_data$nearest_non_coastal_edge_dist <- NA
        
    }
    
    
  ## put the coords instead of geom so we keep the spatial information about the sites but can save the file as a .csv rather than shapefile
  species_data$Site_Longitude_x_behr <- st_coordinates(species_data)[,1]
  species_data$Site_Latitude_y_behr <- st_coordinates(species_data)[,2]
  
  
  write.csv(st_drop_geometry(species_data), paste('data/processed/Edge_dist_data/', species, '.csv', sep = ''))
  
  rm(list = setdiff(ls(), c('ranges','buffered_coastline','data','behr', 'species_list')))

  }, error = function(e) {
    print(paste("Error:", conditionMessage(e), "\n"))
  })
  
  
  
}
