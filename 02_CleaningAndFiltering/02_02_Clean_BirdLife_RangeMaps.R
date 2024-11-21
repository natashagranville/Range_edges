#### Birdlife range data ----------------------

library(sf)

behr <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

data <- read.csv("data/processed/PresAbs/FOREST_COMBINED_PresAbs_Americas.csv")
data <- st_as_sf(data, coords = c("Site_Longitude_x", "Site_Latitude_y"), crs = 4326)


##### Subset BOTW --------------------------

# BOTW - birds of the world
# Data requested from BirdLife

BOTW <- st_read("path_to_BOTW_file.gdb")

BOTW$sci_nam <- gsub(" ", "_", BOTW$sci_nam)

ranges <- subset(BOTW, BOTW$sci_name %in% data$Final_name)

st_write(ranges, "data/raw/BirdLife_ranges_Americas/BirdLife_ranges_Americas.shp", delete_layer = TRUE) 



##### Clean and validate range geometries ----------
# From Orme et al 2019: https://osf.io/cyfqj 
ranges <- st_read("data/raw/BirdLife_ranges_Americas/BirdLife_ranges_Americas.shp")

# Reduce the main set of maps to the selected species, exclude introductions and vagrants and extinct or possibly extinct species
# and drop all but the SCINAME column
ranges_native_reintroduced <- subset(ranges, 
                                       origin %in% c(1,2) & # native or reintroduced,
                                       presenc %in% c(1,3) & # extant or possibly extant,
                                        sci_nam %in% data$Final_name,
                                       select = sci_nam)

# reproject to the analysis spatial reference system before validating.
ranges_native_reintroduced <- st_transform(ranges_native_reintroduced, behr)

# Quite a few invalid features - get the reasons and reduce to the reason not location
check <- st_is_valid(ranges_native_reintroduced, reason=TRUE)
check_main <- sub('\\[[0-9\\. -]+\\]', '', check)
# Self intersection and ring self-intersections
table(check_main)


# Make the ranges valid. Note that this is specific to a particular projection.
ranges_native_reintroduced <- st_make_valid(ranges_native_reintroduced)

# Condense this to get one feature per species:
# - get a list with of single species sf dataframes
ranges_native_reintroduced  <- split(ranges_native_reintroduced, ranges_native_reintroduced$sci_nam)

# - apply st_union across those to get a single (multi)polyline per species
ranges_native_reintroduced  <- lapply(ranges_native_reintroduced, st_union)

# - put them back together into an sf object
ranges_native_reintroduced_sfc <- do.call(c, ranges_native_reintroduced)
ranges_native_reintroduced  <- st_as_sf(data.frame(sci_nam = names(ranges_native_reintroduced),
                                                     geometry = ranges_native_reintroduced_sfc))

st_crs(ranges_native_reintroduced) <- behr

# save the file
st_write(ranges_native_reintroduced, 'data/processed/Cleaned_BirdLife_ranges_Americas/Cleaned_BirdLife_ranges_Americas.shp', delete_layer=TRUE)

