#### Poleward vs equatorward direction ---------------------------------------------

library(dplyr)
library(fasterize)
library(sf)
library(terra)
library(units)

behr <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

#### Get data -------------

##### Range polygons -------------

# - Read in range data
ranges <- st_read("data/processed/Cleaned_BirdLife_ranges_Americas/Cleaned_BirdLife_ranges_Americas.shp")

##### Bird count data ----------
data <- read.csv("data/processed/PresAbs/CLEANED_FOREST_COMBINED_PresAbs_Americas.csv")

data <- st_as_sf(data, coords = c('Site_Longitude_x_behr', 'Site_Latitude_y_behr'), crs = behr)


length(unique(data$Final_name))
length(unique(data$Study))
length(unique(data$SiteID))


# Get list of species names
species_list <- unique(data$Final_name)

##### Bioclim data --------

# - Read in bioclim data
bioclim_hist <- rast("data/processed/BioClim/BioClim_hist_2.5.tif")

#### Buffered coastline -------
buffered_coastline <- st_read("data/processed/buffered_coastline/buffered_coastline.shp")


## remove the current distance columns
# they're not wrong, it's just that for this particular analysis, we convert the range to a raster
# so we calculating the distance to the nearest edge _pixel_
data <- subset(data, select = -c(nearest_edge_dist,nearest_non_coastal_edge_dist,nearest_non_coastal_edge_dist_km))
colnames(data)

for (i in 1:length(species_list)){
  
  tryCatch({
    
    species <- species_list[i]
    
    data_species <- subset(data, Final_name == species)
    
    #### 1 - Rasterise range -------------------------------------------------------
    # - Select range for this species
    species_range <- subset(ranges, sci_nam == species)
    
    plot(st_geometry(species_range))
    
    # - Get extent of this species range
    extent <- st_bbox(species_range)
    
    # - Crop climate raster
    bioclim_hist_cropped <- terra::crop(bioclim_hist, extent)
    
    # - Create target raster
    target <- rast(res = res(bioclim_hist_cropped), ext = ext(bioclim_hist_cropped), crs = crs(bioclim_hist_cropped))
    
    # - Rasterize range
    range_raster <- as(fasterize(species_range, as(target, 'Raster')), 'SpatRaster')
    
    
    plot(range_raster)
    plot(st_geometry(data_species), add = T, pch = 4)
    
    # - Extend range raster
    # so that we can measure distance to range edge for species observed outside the range
    range_raster <- extend(range_raster,  st_bbox(data_species))
    
    plot(range_raster)
    plot(st_geometry(data_species), add = T, pch = 4)
    
    #### 2 - Get coordinates of inland and coastal range edge cells ----------------
    
    # - Cast species range to multilinestring
    species_range_outline <- st_cast(species_range, 'MULTILINESTRING', do_split = FALSE)
    
    # - Get coordinates of inland range edge cells 
    inland_edges <- st_intersection(species_range_outline, buffered_coastline)
    
    plot(st_geometry(inland_edges), add = T)
    
    inland_edge_coords <- terra::extract(range_raster, inland_edges, xy = T)
    inland_edge_coords_sf <- st_as_sf(inland_edge_coords, coords = c('x', 'y'), crs = behr)
    
    plot(st_geometry(inland_edge_coords_sf), add = T)
    
    # - Get coordinates of coastal range edge cells 
    coastal_edges <- st_difference(species_range_outline, buffered_coastline)
    plot(st_geometry(coastal_edges), add = T, col = 'blue')
    
    if(nrow(coastal_edges) != 0){
      
      coastal_edge_coords <- terra::extract(range_raster, coastal_edges, xy = T)
      coastal_edge_coords_sf <- st_as_sf(coastal_edge_coords, coords = c('x', 'y'), crs = behr)
      plot(st_geometry(coastal_edge_coords_sf), add = T, col = 'blue')
      
      
    }
    
    
    
    
    #### 3 - Calculate distance to inland range edges ------------------------------
    
    nearest_inland_edges_indices <- st_nearest_feature(data_species, inland_edge_coords_sf)
    nearest_inland_edges_geoms <- inland_edge_coords_sf[nearest_inland_edges_indices,]
    
    inland_edge_dists <- drop_units(st_distance(data_species, nearest_inland_edges_geoms, by_element = TRUE))
    
    in_range <- drop_units(st_distance(data_species, species_range)) # Check if species is in the range
    final_inland_edge_dists <- inland_edge_dists * ((in_range == 0)* 2 - 1) # Negative distances for sites outside range
    
    data_species$inland_edge_dists <- final_inland_edge_dists[,1]
    
    
    #### 4 - Calculate distance to coastal range edges ------------------------------
    
    if(nrow(coastal_edges) != 0){
      coastal_edge_dists <- drop_units(st_distance(data_species, coastal_edges))
      
      data_species$coastal_edge_dists <- coastal_edge_dists[,1]
      
    } else {
      
      data_species$coastal_edge_dists <- NA
      
    }
    
    
    
    
    #### 5 -  Poleward vs equatorward direction of inland edges -----------
    
    ## latitude of site
    data_species$site_lat <- st_coordinates(data_species$geometry)[, 2]
    
    ## latitude of edges
    data_species$edge_lat <- st_coordinates(nearest_inland_edges_geoms)[,2]
    
    plot(st_geometry(nearest_inland_edges_geoms), add = T, col = "red")
    
    ## categorise poleward and equatorward
    
    data_species$edge_direction <- NA
    
    for (i in 1:nrow(data_species)){
      
      if(!is.na(data_species$site_lat[i])){
        
        if (data_species$site_lat[i] < 0){ # southern hemisphere
          
          data_species$edge_direction[i] <- ifelse(data_species$edge_lat[i] > data_species$site_lat[i], 'equatorward_direction', 'poleward_direction')
          
        } else if (data_species$site_lat[i] > 0){ # northern hemisphere
          
          data_species$edge_direction[i] <- ifelse(data_species$edge_lat[i] < data_species$site_lat[i], 'equatorward_direction', 'poleward_direction')
          
        }
      }
      
    }
    
    
    
    #### 6 - Poleward or equatorward half --------------------
    
    # - get centroid of range
    centroid <- st_centroid(species_range)
    plot(st_geometry(centroid), add = T, pch = 11, col = 'blue')
    
    # - get centroid latitude
    midpoint_lat <- st_coordinates(centroid)[,2]
    
    # - decide if the site is in the poleward or equatorward half of species' range
    
    data_species$latitude_half <- NA
    
    for (i in 1:nrow(data_species)){
      
      if(!is.na(data_species$site_lat[i])){
        
        if (data_species$site_lat[i] < 0){ # southern hemisphere
          
          data_species$latitude_half[i] <- ifelse(data_species$site_lat[i] > midpoint_lat, 'equatorward_half', 'poleward_half')
          
        } else if (data_species$site_lat[i] > 0){ # northern hemisphere
          
          data_species$latitude_half[i] <- ifelse(data_species$site_lat[i] < midpoint_lat, 'equatorward_half', 'poleward_half')
          
        }
      }
      
    }
    
    
    #### 7 - Put NA where the difference in latitude with the is not detectable at this resolution ------------
    
    # the resolution
    res(range_raster)
    
    # make edge_direction NA if the difference in latitude between edge and site is less than the resolution
    data_species$edge_direction <- ifelse(abs(data_species$site_lat - data_species$edge_lat) < res(range_raster)[2], 
                                          NA, data_species$edge_direction)

    # how many is that
    sum(is.na(data_species))/nrow(data_species)
    
    #### 8 - Save data -----------
    
    ## put the coords instead of geom so we keep the spatial information about the sites but can save the file as a .csv rather than shapefile
    colnames(data_species)
    data_species <- subset(data_species, select = - c(site_lat, edge_lat))
    
    data_species$Site_Longitude_x <- st_coordinates(data_species)[,1]
    data_species$Site_Latitude_y <- st_coordinates(data_species)[,2]
    
    write.csv(st_drop_geometry(data_species), paste('data/processed/HardVsSoft_PolewardVsEquatorward_data/', species, '.csv', sep = ''))
    
    rm(list = setdiff(ls(), c('ranges','buffered_coastline','data', 'behr', 'bioclim_hist', 'species_list')))
    
  }, error = function(e) {
    print(paste("Error:", conditionMessage(e), "\n"))
    
  })
  
}
