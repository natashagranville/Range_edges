### NOTE: This only needs to be done once. Save the rasters in a raw data folder.

## Download Hansen et al tree cover data
# https://storage.googleapis.com/earthenginepartners-hansen/GFC-2022-v1.10/download.html

library(gfcanalysis)
library(sf)
library(dplyr)

#### Get sites -----------------------------------------------------------------
data <- read.csv("data/processed/PresAbs/CLEANED_FOREST_COMBINED_PresAbs_Americas.csv")

sites <- data %>% distinct(SiteID, .keep_all = TRUE) %>% select(c('geometry', 'SiteID'))

## Get tiles that cover my sites
tiles <- calc_gfc_tiles(sites)

## Plot tiles
library(maps)
map('world',col="grey85", fill=TRUE, bg="white", lwd=0.05,  xlim=c(-160,-20), ylim = c(-60, 75))
map.axes()
plot(st_geometry(tiles), add = T)


#### Download Hansen forest cover data -----------------------------------------

# NOTE: This is error-prone. You will likely have to check through the folder to see if all the tiles have downloaded
# Then just download the missing ones from this link https://storage.googleapis.com/earthenginepartners-hansen/GFC-2022-v1.10/download.html

for (i in 1:nrow(tiles)){
  
  tryCatch({
    
    # download the file for one tile
    download_tiles(tiles = tiles[i,], 
                   output_folder = "data/raw/Hansen_ForestCover", 
                   images = c("treecover2000", "datamask"),
                   dataset = "GFC-2022-v1.10")
    
    
  }, error = function(e) {
 
    print(paste("Error:", conditionMessage(e), "\n"))
  })
  
}

