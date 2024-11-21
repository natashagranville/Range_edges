### NOTE: This only needs to be done once. Save the shapefile in a processde data folder.

#### Get buffered coastline ----------------------------------------------------
# downloaded from: https://osf.io/4pbzt/files/osfstorage


library(sf)

behr <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

sa_coast <- st_read("data/raw/GSHHS_continental_coastlines/gshhs_south_america.shp")
na_coast <- st_read("data/raw/GSHHS_continental_coastlines/gshhs_north_america.shp")

# unify the two continents with st_union
coast <- st_union(sa_coast, na_coast)

# transform to behrmann projection
coast <- st_transform(coast, behr)

# 10km buffer
coast_buff <- st_buffer(coast, -10000)

# remove attribute columns
coast_buff <- coast_buff %>% select(geometry)

st_write(coast_buff, "data/processed/buffered_coastline/buffered_coastline.shp", append = FALSE)

