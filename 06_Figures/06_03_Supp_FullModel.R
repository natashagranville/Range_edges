#### Figure for full model ------------------

library(ggplot2)
library(glmmTMB)
library(metR)
library(sf)

behr <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

data <- read.csv('data/processed/Final_data.csv')

load("results/full_model.Rdata")

#### Map ---------------------------
data <- st_as_sf(data, coords = c('Site_Longitude_x', 'Site_Latitude_y'), crs = behr, remove = F)

buffered_coastline <- st_read("data/processed/buffered_coastline/buffered_coastline.shp")

full_map <- ggplot() +
  geom_sf(data = buffered_coastline, fill = 'grey80', col = NA) +
  geom_sf(data = data, size = 4) +
  theme_void()
full_map
ggsave('figures/full_map.jpg')


#### Heatmap --------------------------------------
pred <- expand.grid(fp70_600m = seq(min(data$fp70_600m, na.rm = TRUE), max(data$fp70_600m, na.rm = TRUE), length.out = 100),
                    inland_edge_dists_km = seq(-200, max(data$inland_edge_dists_km, na.rm = TRUE), length.out = 100)) 

# square-root transformation for dist to edge
pred <- pred %>% mutate(sqrt_inland_edge_dists_km = sign(inland_edge_dists_km) * sqrt(abs(inland_edge_dists_km)))

# make predictions on this data
pred$Study <- NA
pred$Final_name <- NA

pred <- mutate(pred, pred = unlist(predict(object = full_model, newdata = pred, type = "response", re.form = NA)))

full_model_plot <- ggplot(mutate(pred, Occupancy = as.numeric(pred)),
                      aes(x = fp70_600m, y = inland_edge_dists_km, z = Occupancy)) +
  geom_raster(aes(fill = Occupancy),) + 
  geom_contour( colour = 'black') +
  xlab('Forest cover proportion') + 
  ylab('Distance to range edge (km)') +
  scale_fill_gradient2(low = "white", mid = "royalblue",high = "darkblue",
                       limits = c(0,1), midpoint =0.5) + 
  theme_classic() +
  geom_text_contour(skip = 0, nudge_y = 60, nudge_x = 0, size = 5, check_overlap = T, colour = 'black') +
  theme(axis.text = element_text(size = 16, colour = 'black'),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        legend.position = 'none')
full_model_plot
ggsave('figures/full_model_plot.jpg')


#### Simplified heatmap ---------------


pred_labels <- expand.grid(fp70_600m = c(0.1,0.5,0.9),
                           sqrt_inland_edge_dists_km = c(9,26,43))
pred_labels$predicted_occurrence <- predict(full_model, pred_labels, type = "response", re.form = NA)

pred_labels$predicted_occurrence_rounded <- round(pred_labels$predicted_occurrence, 1)


pred_dist1 <- expand.grid(fp70_600m = seq(min(data$fp70_600m, na.rm = TRUE), max(data$fp70_600m, na.rm = T), length.out = 100),
                          sqrt_inland_edge_dists_km = 9) 
pred_dist1$pred <- predict(full_model, pred_dist1, type = "response", re.form = NA)

pred_dist2 <- expand.grid(fp70_600m = seq(min(data$fp70_600m, na.rm = TRUE), max(data$fp70_600m, na.rm = T), length.out = 100),
                          sqrt_inland_edge_dists_km = 26) 
pred_dist2$pred <- predict(full_model, pred_dist2, type = "response", re.form = NA)

pred_dist3 <- expand.grid(fp70_600m = seq(min(data$fp70_600m, na.rm = TRUE), max(data$fp70_600m, na.rm = T), length.out = 100),
                          sqrt_inland_edge_dists_km = 43) 
pred_dist3$pred <- predict(full_model, pred_dist3, type = "response", re.form = NA)

pred_dist <- rbind(pred_dist1, pred_dist2, pred_dist3)

heatmap_simplified <- ggplot(data = pred_dist, aes(x = fp70_600m, y = sqrt_inland_edge_dists_km)) +
  geom_point(aes(colour = pred), size = 15, shape = 15) +
  scale_colour_gradient2(low = "white", mid = "royalblue", high = "darkblue",
                         limits = c(0,1), midpoint =0.5)  +
  theme_classic() +
  scale_y_continuous(limits = c(6, max(data$sqrt_inland_edge_dists_km, na.rm = T)),
                     breaks = c(9,26,43),
                     labels = c(80,700,1800)) +
  geom_text(data = pred_labels, aes(x = fp70_600m, y = sqrt_inland_edge_dists_km, label = predicted_occurrence_rounded), size = 10) +
  labs(x = 'Forest cover proportion', y = 'Distance to range edge (km)', colour = 'Bird\noccupancy\nprobability') +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20, colour = 'black'),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20))
heatmap_simplified
ggsave("figures/heatmap_simplified.jpg")
