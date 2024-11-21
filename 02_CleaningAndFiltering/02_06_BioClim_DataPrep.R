### NOTE: This only needs to be done once. Save the raster in a processed data folder.

#### BioClim data ------------------
# https://www.worldclim.org/data/bioclim.html

# The BioClim raster is just used here as a template for rasterising the geographic ranges
library(geodata)
library(terra)

behr <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

## Download bioclim data
bioclim_hist <- worldclim_global(var="bio",res=2.5, path = "data/raw/BioClim")

## Set names
bioclim_names <- paste0('bio', 1:19)
names(bioclim_hist) <- bioclim_names

## Re-project
bioclim_hist <- bioclim_hist %>% project(behr, method = 'bilinear') 

## Crop to extent of all ranges
# - Read in range data
ranges <- st_read("data/processed/Cleaned_BirdLife_ranges_Americas/Cleaned_BirdLife_ranges_Americas.shp")
# - Crop
bioclim_hist_cropped_all <- crop(bioclim_hist, st_bbox(ranges))

## Save
writeRaster(bioclim_hist_cropped_all, "data/processed/BioClim/BioClim_hist_2.5.tif")
