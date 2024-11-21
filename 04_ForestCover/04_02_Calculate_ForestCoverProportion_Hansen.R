##### Get forest percent #######
# Tree cover and datamask tiles (Hansen et al) downloaded from : https://storage.googleapis.com/earthenginepartners-hansen/GFC-2022-v1.10/download.html

library(dplyr)
library(gfcanalysis)
library(sf)
library(terra)

behr <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"


##### Get buffered sites -------------------------------------------------------

data <- read.csv("data/processed/PresAbs/CLEANED_FOREST_COMBINED_PresAbs_Americas.csv")


data <- st_as_sf(data, coords = c('Site_Longitude_x_behr', 'Site_Latitude_y_behr'), crs = behr)


sites <- data %>% distinct(SiteID, .keep_all = TRUE) %>% dplyr::select(c('geometry', 'SiteID'))

# Draw buffer around sites
buffered_sites_600m <- st_buffer(sites, 600) 

# Get tiles
tiles <- calc_gfc_tiles(buffered_sites_600m)

# Change crs of sites
buffered_sites_600m <- st_transform(buffered_sites_600m, st_crs(tiles))



### Functions for formatting coordinates for tile names
convert_coord <- function(coord, type) {
  if (coord < 0) {
    coord <- abs(coord)
    direction <- ifelse(type == "lon", "W", "S")
  } else {
    direction <- ifelse(type == "lon", "E", "N")
  }
  
  if (type == "lon") {
    formatted_coord <- sprintf("%03d%s", coord, direction)
  } else {
    formatted_coord <- sprintf("%02d%s", coord, direction)
  }
  
  return(formatted_coord)
}

convert_coords <- function(coords) {
  
  lon <- coords[1]
  lat <- coords[2]
  
  lon_converted <- convert_coord(lon, "lon")
  lat_converted <- convert_coord(lat, "lat")
  
  return(c(lon_converted, lat_converted))
}


## Function wrapper for calculating forest cover
calculate_forest_cover_proportion <- function(tiles, buffered_sites, buffer, threshold){
  
  output_df <- data.frame()
  
  for (i in 1:nrow(tiles)){
    
    tile <- tiles[i,]
    
    print(paste0(" --------- Calculating forest cover for sites in tile ", i, " out of ", nrow(tiles), " --------- "))
    
    ## Get sites that intersect with the tile
    buffered_sites_tile <- st_intersection(buffered_sites, tile)
    
    #plot(tile, axes = T)
    #plot(st_geometry(buffered_sites_tile), add = T)
    
    ## Get coordinate of top-left corner of tile
    tile_xmin <- st_bbox(tile)[1]
    tile_ymax <- st_bbox(tile)[4]
    coords <- c(tile_xmin, tile_ymax)
    converted_coords <- convert_coords(coords)
    converted_coords
    
    ## Read in the raster files for this tile
    
    # Read in the raster files for this tile
    treecover_rast <- rast(paste0("data/raw/Hansen_ForestCover/Hansen_GFC-2022-v1.10_treecover2000_", converted_coords[2], "_", converted_coords[1], ".tif"))
    datamask_rast <- rast(paste0("data/raw/Hansen_ForestCover/Hansen_GFC-2022-v1.10_datamask_", converted_coords[2], "_", converted_coords[1], ".tif"))
    
    #plot(treecover_rast)
    #plot(st_geometry(buffered_sites_tile), add = T)
    
    
    
    ## Check whether any buffered sites are on the border of this tile and another tile
    buffered_sites_tile_intersection <- st_intersection(buffered_sites, tile)
    buffered_sites_tile_intersection$area <- round(st_area(buffered_sites_tile_intersection))
    
    buffered_sites_orig <- subset(buffered_sites, SiteID %in% buffered_sites_tile_intersection$SiteID)
    buffered_sites_orig$area <- round(st_area(buffered_sites_orig))
    
    is_complete <- buffered_sites_tile_intersection$area ==  buffered_sites_orig$area
    
    #plot(st_geometry(tile), axes = T)
    #plot(st_geometry(buffered_sites_tile_intersection), add = T)
    #plot(st_geometry(buffered_sites_orig), add = T)
    
    ## If any buffered sites overlap >1 tile, merge these tiles
    if(any(is_complete == F)){
      
      tiles_to_merge <- calc_gfc_tiles(buffered_sites_orig)
      
      treecover_rast_list <- list()
      datamask_rast_list <- list()
      
      for (i in 1:nrow(tiles_to_merge)) {
        
        # Get coordinate of top-left corner of tile
        tile_i <- tiles_to_merge[i,]
        tile_i_xmin <- st_bbox(tile_i)[1]
        tile_i_ymax <- st_bbox(tile_i)[4]
        coords_i <- c(tile_i_xmin, tile_i_ymax)
        converted_coords_i <- convert_coords(coords_i)
        converted_coords_i
        
        # Read in the raster files for this tile
        treecover_rast_i <- rast(paste0("data/raw/Hansen_ForestCover/Hansen_GFC-2022-v1.10_treecover2000_", converted_coords_i[2], "_", converted_coords_i[1], ".tif"))
        datamask_rast_i <- rast(paste0("data/raw/Hansen_ForestCover/Hansen_GFC-2022-v1.10_datamask_", converted_coords_i[2], "_", converted_coords_i[1], ".tif"))
        
        # Join this with the list of tiles
        treecover_rast_list <- list(treecover_rast_list, treecover_rast_i)
        datamask_rast_list <- list(datamask_rast_list, datamask_rast_i)
        
      }
      
      # Remove null element
      treecover_rast_list[[1]] <- treecover_rast_list[[1]][[2]] 
      datamask_rast_list[[1]] <- datamask_rast_list[[1]][[2]] 
      
      # Merge raster tiles
      treecover_rast <- do.call(merge, treecover_rast_list)
      datamask_rast <- do.call(merge, datamask_rast_list)
      
    }
    
    
    
    
    ## Calculate forest cover at each buffered site
    
    # Initialise the output dataframe
    buffered_sites_with_forest_proportions <- data.frame()
    
    for (i in 1:nrow(buffered_sites_tile)) {
      
      buffered_site <- buffered_sites_tile[i,]
      
      tryCatch({
        
        ## Treecover raster
        treecover <- crop(treecover_rast, st_bbox(buffered_site))
        
        ## Datamask
        datamask <- crop(datamask_rast, st_bbox(buffered_site))
        
      }, error = function(e) {
        print(paste("Error:", conditionMessage(e), "\n"))
        
      })
      
      ## Mask (convert to NA) any pixels in treecover raster that are 0 (no data) or 2 (water) in datamask
      treecover_masked <- terra::mask(treecover, datamask, maskvalues = c(0,2))
      
      ## Threshold to convert to binary map of forest/non-forest
      forest_map <- treecover_masked > threshold 
      
      #plot(forest_map, col = terrain.colors(2, rev = T))
      
      ## Get forest proportion within the buffer 
      forest_proportion <- sum(values(forest_map) == TRUE) / length(values(forest_map))
      
      ## Add to dataframe with site name
      buffered_site_tile_with_forest_proportions <- cbind(st_drop_geometry(buffered_site), forest_proportion)
      
      ## Add this row to the main dataframe with all the buffered_sites
      buffered_sites_with_forest_proportions <- rbind(buffered_sites_with_forest_proportions, buffered_site_tile_with_forest_proportions)
      
      
    }
    
    
    ## Rename forest proportion column
    names(buffered_sites_with_forest_proportions)[names(buffered_sites_with_forest_proportions) == 'forest_proportion'] <- paste('fp', threshold, '_', buffer, 'm', sep = '')
    
    output_df <- rbind(output_df, buffered_sites_with_forest_proportions)
    
  }
  
  
  return(output_df)
  
}



#### Calculate forest cover proportion  ------

fp10_600m <- calculate_forest_cover_proportion(tiles = tiles, buffered_sites = buffered_sites_600m, threshold = 10, buffer = 600)
write.csv(fp10_600m, "data/processed/Forest_cover_proportions_Hansen/Forest10_proportions_600m.csv")

fp20_600m <- calculate_forest_cover_proportion(tiles = tiles, buffered_sites = buffered_sites_600m, threshold = 20, buffer = 600)
write.csv(fp20_600m, "data/processed/Forest_cover_proportions_Hansen/Forest20_proportions_600m.csv")

fp30_600m <- calculate_forest_cover_proportion(tiles = tiles, buffered_sites = buffered_sites_600m, threshold = 30, buffer = 600)
write.csv(fp30_600m, "data/processed/Forest_cover_proportions_Hansen/Forest30_proportions_600m.csv")

fp40_600m <- calculate_forest_cover_proportion(tiles = tiles, buffered_sites = buffered_sites_600m, threshold = 40, buffer = 600)
write.csv(fp40_600m, "data/processed/Forest_cover_proportions_Hansen/Forest40_proportions_600m.csv")

fp50_600m <- calculate_forest_cover_proportion(tiles = tiles, buffered_sites = buffered_sites_600m, threshold = 50, buffer = 600)
write.csv(fp50_600m, "data/processed/Forest_cover_proportions_Hansen/Forest50_proportions_600m.csv")

fp60_600m <- calculate_forest_cover_proportion(tiles = tiles, buffered_sites = buffered_sites_600m, threshold = 60, buffer = 600)
write.csv(fp60_600m, "data/processed/Forest_cover_proportions_Hansen/Forest60_proportions_600m.csv")

fp70_600m <- calculate_forest_cover_proportion(tiles = tiles, buffered_sites = buffered_sites_600m, threshold = 70, buffer = 600)
write.csv(fp70_600m, "data/processed/Forest_cover_proportions_Hansen/Forest70_proportions_600m.csv")

fp80_600m <- calculate_forest_cover_proportion(tiles = tiles, buffered_sites = buffered_sites_600m, threshold = 80, buffer = 600)
write.csv(fp80_600m, "data/processed/Forest_cover_proportions_Hansen/Forest80_proportions_600m.csv")

fp90_600m <- calculate_forest_cover_proportion(tiles = tiles, buffered_sites = buffered_sites_600m, threshold = 90, buffer = 600)
write.csv(fp90_600m, "data/processed/Forest_cover_proportions_Hansen/Forest90_proportions_600m.csv")




