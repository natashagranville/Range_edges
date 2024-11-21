#### Figure for hard vs soft -------------------

library(ggplot2)
library(glmmTMB)
library(metR)
library(sf)

behr <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

data <- read.csv('data/processed/Final_data.csv')

data_coast <- subset(data, sqrt_inland_edge_dists_km > sqrt_coastal_edge_dists_km)

load("results/HardVsSoft/hard_model.Rdata")
load("results/HardVsSoft/soft_model.Rdata")

#### Map ---------------------------
data_coast <- st_as_sf(data_coast, coords = c('Site_Longitude_x', 'Site_Latitude_y'), crs = behr, remove = F)

buffered_coastline <- st_read("data/processed/buffered_coastline/buffered_coastline.shp")

coast_subset_map <- ggplot() +
  geom_sf(data = buffered_coastline, fill = 'grey80', col = NA) +
  geom_sf(data = data_coast, size = 4) +
  theme_void()
coast_subset_map
ggsave('figures/HardVsSoft/coast_subset_map.jpg')


#### Heatmap --------------------------------------
##### Hard ---------------------------
pred <- expand.grid(fp70_600m = seq(min(data_coast$fp70_600m, na.rm = TRUE), max(data_coast$fp70_600m, na.rm = TRUE), length.out = 100),
                    coastal_edge_dists_km = seq(0, max(data_coast$coastal_edge_dists_km, na.rm = TRUE), length.out = 100)) 

# square-root transformation for dist to edge
pred <- pred %>% mutate(sqrt_coastal_edge_dists_km = sign(coastal_edge_dists_km) * sqrt(abs(coastal_edge_dists_km)))

# make predictions on this data
pred$Study <- NA
pred$Final_name <- NA

pred <- mutate(pred, pred = unlist(predict(object = hard_model, newdata = pred, type = "response", re.form = NA)))

hard_model_plot <- ggplot(mutate(pred, Occupancy = as.numeric(pred)),
                          aes(x = fp70_600m, y = coastal_edge_dists_km, z = Occupancy)) +
  geom_raster(aes(fill = Occupancy),) + 
  geom_contour( colour = 'black') +
  xlab('Forest cover proportion') + 
  ylab('Distance to range edge (km)') +
  ggtitle("Hard edges") +
  scale_fill_gradient2(low = "white", mid = "royalblue",high = "darkblue",
                       limits = c(0,1), midpoint =0.5) + 
  theme_classic() +
  geom_text_contour(skip = 0, nudge_y = 20, nudge_x = 0, size = 4, check_overlap = T, colour = 'black') +
  theme(axis.text = element_text(size = 16, colour = 'black'),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        legend.position = 'none')
hard_model_plot
ggsave('figures/HardVsSoft/hard_model_plot.jpg')



##### Soft ---------------------------
pred <- expand.grid(fp70_600m = seq(min(data_coast$fp70_600m, na.rm = TRUE), max(data_coast$fp70_600m, na.rm = TRUE), length.out = 100),
                    inland_edge_dists_km = seq(-200, max(data_coast$inland_edge_dists_km, na.rm = TRUE), length.out = 100)) 

# square-root transformation for dist to edge
pred <- pred %>% mutate(sqrt_inland_edge_dists_km = sign(inland_edge_dists_km) * sqrt(abs(inland_edge_dists_km)))

# make predictions on this data
pred$Study <- NA
pred$Final_name <- NA

pred <- mutate(pred, pred = unlist(predict(object = soft_model, newdata = pred, type = "response", re.form = NA)))

soft_model_plot <- ggplot(mutate(pred, Occupancy = as.numeric(pred)),
                          aes(x = fp70_600m, y = inland_edge_dists_km, z = Occupancy)) +
  geom_raster(aes(fill = Occupancy),) + 
  geom_contour( colour = 'black') +
  xlab('Forest cover proportion') + 
  ylab('Distance to range edge (km)') +
  ggtitle("Soft edges") +
  scale_fill_gradient2(low = "white", mid = "royalblue",high = "darkblue",
                       limits = c(0,1), midpoint =0.5) + 
  theme_classic() +
  geom_text_contour(skip = 0, nudge_y = 80, nudge_x = 0, size = 5, check_overlap = T, colour = 'black') +
  theme(axis.text = element_text(size = 16, colour = 'black'),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        legend.position = 'none')
soft_model_plot
ggsave('figures/HardVsSoft/soft_model_plot.jpg')




