#### Figure for Equatorward vs Poleward -------------------

library(ggplot2)
library(glmmTMB)
library(metR)
library(sf)

behr <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

data <- read.csv('data/processed/Final_data.csv')

equatorward_data <- subset(data, edge_direction == "equatorward_direction")
poleward_data <- subset(data, edge_direction == "poleward_direction")

load("results/EquatorwardVsPoleward/equatorward_model.Rdata")
load("results/EquatorwardVsPoleward/poleward_model.Rdata")

#### Map ---------------------------
data <- st_as_sf(data, coords = c('Site_Longitude_x', 'Site_Latitude_y'), crs = behr, remove = F)

buffered_coastline <- st_read("data/processed/buffered_coastline/buffered_coastline.shp")

data_map <- ggplot() +
  geom_sf(data = buffered_coastline, fill = 'grey80', col = NA) +
  geom_sf(data = data, size = 3) +
  theme_void()
data_map
ggsave('figures/full_map.jpg')


#### Explainer heatmap ----------------------------

##### Equatorward --------------------------------
pred_equatorward_dist_labels <- expand.grid(fp70_600m = c(0.1,0.5,0.9),
                                     inland_edge_dists_km = c(200,600,1000))

pred_equatorward_dist_labels <- pred_equatorward_dist_labels %>% mutate(sqrt_inland_edge_dists_km = sign(inland_edge_dists_km) * sqrt(abs(inland_edge_dists_km)))

pred_equatorward_dist_labels$predicted_occurrence <- predict(equatorward_model, pred_equatorward_dist_labels, type = "response", re.form = NA)

pred_equatorward_dist_labels$predicted_occurrence_rounded <- round(pred_equatorward_dist_labels$predicted_occurrence, 1)


pred_equatorward_dist_200 <- expand.grid(inland_edge_dists_km = 200,
                                  fp70_600m = seq(min(equatorward_data$fp70_600m, na.rm = TRUE), max(equatorward_data$fp70_600m, na.rm = T), length.out = 1000)) 
pred_equatorward_dist_200 <- pred_equatorward_dist_200 %>% mutate(sqrt_inland_edge_dists_km = sign(inland_edge_dists_km) * sqrt(abs(inland_edge_dists_km)))
pred_equatorward_dist_200 <- mutate(pred_equatorward_dist_200, pred = unlist(predict(object = equatorward_model, newdata = pred_equatorward_dist_200, type = "response", re.form = NA)))


pred_equatorward_dist_600 <- expand.grid(inland_edge_dists_km = 600,
                                  fp70_600m = seq(min(equatorward_data$fp70_600m, na.rm = TRUE), max(equatorward_data$fp70_600m, na.rm = T), length.out = 1000)) 
pred_equatorward_dist_600 <- pred_equatorward_dist_600 %>% mutate(sqrt_inland_edge_dists_km = sign(inland_edge_dists_km) * sqrt(abs(inland_edge_dists_km)))
pred_equatorward_dist_600 <- mutate(pred_equatorward_dist_600, pred = unlist(predict(object = equatorward_model, newdata = pred_equatorward_dist_600, type = "response", re.form = NA)))


pred_equatorward_dist_1000 <- expand.grid(inland_edge_dists_km = 1000,
                                   fp70_600m = seq(min(equatorward_data$fp70_600m, na.rm = TRUE), max(equatorward_data$fp70_600m, na.rm = T), length.out = 1000)) 
pred_equatorward_dist_1000 <- pred_equatorward_dist_1000 %>% mutate(sqrt_inland_edge_dists_km = sign(inland_edge_dists_km) * sqrt(abs(inland_edge_dists_km)))
pred_equatorward_dist_1000 <- mutate(pred_equatorward_dist_1000, pred = unlist(predict(object = equatorward_model, newdata = pred_equatorward_dist_1000, type = "response", re.form = NA)))

pred_equatorward_dist <- rbind(pred_equatorward_dist_200, pred_equatorward_dist_600, pred_equatorward_dist_1000)


heatmap_explainer_equatorward_dist <- ggplot(data = pred_equatorward_dist, aes(x = fp70_600m, y = inland_edge_dists_km)) +
  geom_point(aes(colour = pred), size = 15, shape = 15) +
  scale_colour_gradient2(low = "white", mid = "royalblue",high = "darkblue",
                         limits = c(0,1), midpoint = 0.5) + 
  theme_classic() +
  scale_y_continuous(limits = c(0, 1200), 
                     breaks = c(200,600,1000)) +
  geom_text(data = pred_equatorward_dist_labels, aes(x = fp70_600m, y = inland_edge_dists_km, label = predicted_occurrence_rounded), size = 10) +
  labs(x = 'Forest cover proportion', y = 'Distance to range edge (km)', colour = 'Occupancy\nprobability') +
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(size = 20, colour = 'black'),
        legend.position = "none",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20))
heatmap_explainer_equatorward_dist
ggsave("figures/heatmap_explainer_equatorward_dist.jpg")




#### Explainer heatmap ----------------------------

##### Poleward ---------------------------------------
pred_poleward_dist_labels <- expand.grid(fp70_600m = c(0.1,0.5,0.9),
                                            inland_edge_dists_km = c(200,600,1000))

pred_poleward_dist_labels <- pred_poleward_dist_labels %>% mutate(sqrt_inland_edge_dists_km = sign(inland_edge_dists_km) * sqrt(abs(inland_edge_dists_km)))

pred_poleward_dist_labels$predicted_occurrence <- predict(poleward_model, pred_poleward_dist_labels, type = "response", re.form = NA)

pred_poleward_dist_labels$predicted_occurrence_rounded <- round(pred_poleward_dist_labels$predicted_occurrence, 1)


pred_poleward_dist_200 <- expand.grid(inland_edge_dists_km = 200,
                                         fp70_600m = seq(min(poleward_data$fp70_600m, na.rm = TRUE), max(poleward_data$fp70_600m, na.rm = T), length.out = 1000)) 
pred_poleward_dist_200 <- pred_poleward_dist_200 %>% mutate(sqrt_inland_edge_dists_km = sign(inland_edge_dists_km) * sqrt(abs(inland_edge_dists_km)))
pred_poleward_dist_200 <- mutate(pred_poleward_dist_200, pred = unlist(predict(object = poleward_model, newdata = pred_poleward_dist_200, type = "response", re.form = NA)))


pred_poleward_dist_600 <- expand.grid(inland_edge_dists_km = 600,
                                         fp70_600m = seq(min(poleward_data$fp70_600m, na.rm = TRUE), max(poleward_data$fp70_600m, na.rm = T), length.out = 1000)) 
pred_poleward_dist_600 <- pred_poleward_dist_600 %>% mutate(sqrt_inland_edge_dists_km = sign(inland_edge_dists_km) * sqrt(abs(inland_edge_dists_km)))
pred_poleward_dist_600 <- mutate(pred_poleward_dist_600, pred = unlist(predict(object = poleward_model, newdata = pred_poleward_dist_600, type = "response", re.form = NA)))


pred_poleward_dist_1000 <- expand.grid(inland_edge_dists_km = 1000,
                                          fp70_600m = seq(min(poleward_data$fp70_600m, na.rm = TRUE), max(poleward_data$fp70_600m, na.rm = T), length.out = 1000)) 
pred_poleward_dist_1000 <- pred_poleward_dist_1000 %>% mutate(sqrt_inland_edge_dists_km = sign(inland_edge_dists_km) * sqrt(abs(inland_edge_dists_km)))
pred_poleward_dist_1000 <- mutate(pred_poleward_dist_1000, pred = unlist(predict(object = poleward_model, newdata = pred_poleward_dist_1000, type = "response", re.form = NA)))

pred_poleward_dist <- rbind(pred_poleward_dist_200, pred_poleward_dist_600, pred_poleward_dist_1000)


heatmap_explainer_poleward_dist <- ggplot(data = pred_poleward_dist, aes(x = fp70_600m, y = inland_edge_dists_km)) +
  geom_point(aes(colour = pred), size = 15, shape = 15) +
  scale_colour_gradient2(low = "white", mid = "royalblue",high = "darkblue",
                         limits = c(0,1), midpoint = 0.5) + 
  theme_classic() +
  scale_y_continuous(limits = c(0, 1200), 
                     breaks = c(200,600,1000)) +
  geom_text(data = pred_poleward_dist_labels, aes(x = fp70_600m, y = inland_edge_dists_km, label = predicted_occurrence_rounded), size = 10) +
  labs(x = 'Forest cover proportion', y = 'Distance to range edge (km)', colour = 'Occupancy\nprobability') +
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(size = 20, colour = 'black'),
        legend.position = "none",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20))
heatmap_explainer_poleward_dist
ggsave("figures/heatmap_explainer_poleward_dist.jpg")



#### Heatmap --------------------------------------
##### Equatorward ---------------------------
pred <- expand.grid(fp70_600m = seq(min(equatorward_data$fp70_600m, na.rm = TRUE), max(equatorward_data$fp70_600m, na.rm = TRUE), length.out = 100),
                    inland_edge_dists_km = seq(-200, max(equatorward_data$inland_edge_dists_km, na.rm = TRUE), length.out = 100)) 

# square-root transformation for dist to edge
pred <- pred %>% mutate(sqrt_inland_edge_dists_km = sign(inland_edge_dists_km) * sqrt(abs(inland_edge_dists_km)))

# make predictions on this data
pred$Study <- NA
pred$Final_name <- NA

pred <- mutate(pred, pred = unlist(predict(object = equatorward_model, newdata = pred, type = "response", re.form = NA)))

equatorward_model_plot <- ggplot(mutate(pred, Occupancy = as.numeric(pred)),
                          aes(x = fp70_600m, y = inland_edge_dists_km, z = Occupancy)) +
  geom_raster(aes(fill = Occupancy),) + 
  geom_segment(x = 0, xend = 1, y = 190, yend = 190, linetype = "dashed") +
  geom_segment(x = 0, xend = 1, y = 210, yend = 210, linetype = "dashed") +
  geom_segment(x = 0, xend = 1, y = 590, yend = 590, linetype = "dashed") +
  geom_segment(x = 0, xend = 1, y = 610, yend = 610, linetype = "dashed") +
  geom_segment(x = 0, xend = 1, y = 990, yend = 990, linetype = "dashed") +
  geom_segment(x = 0, xend = 1, y = 1010, yend = 1010, linetype = "dashed") +
  geom_contour( colour = 'black') +
  xlab('Forest cover proportion') + 
  ylab('Distance to range edge (km)') +
  ggtitle("Equatorward edges") +
  scale_y_continuous(breaks = c(0,500,1000,1500,2000,2500)) +
  scale_fill_gradient2(low = "white", mid = "royalblue",high = "darkblue",
                       limits = c(0,1), midpoint =0.5) + 
  theme_classic() +
  #geom_text_contour(skip = 0, nudge_y = 80, nudge_x = 0, size = 5, check_overlap = T, colour = 'black') +
  theme(axis.text = element_text(size = 16, colour = 'black'),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        legend.position = 'none')
equatorward_model_plot
ggsave('figures/EquatorwardVsPoleward/equatorward_model_plot.jpg')



##### Poleward ---------------------------
pred <- expand.grid(fp70_600m = seq(min(poleward_data$fp70_600m, na.rm = TRUE), max(poleward_data$fp70_600m, na.rm = TRUE), length.out = 100),
                    inland_edge_dists_km = seq(-200, max(poleward_data$inland_edge_dists_km, na.rm = TRUE), length.out = 100)) 

# square-root transformation for dist to edge
pred <- pred %>% mutate(sqrt_inland_edge_dists_km = sign(inland_edge_dists_km) * sqrt(abs(inland_edge_dists_km)))

# make predictions on this data
pred$Study <- NA
pred$Final_name <- NA

pred <- mutate(pred, pred = unlist(predict(object = poleward_model, newdata = pred, type = "response", re.form = NA)))

poleward_model_plot <- ggplot(mutate(pred, Occupancy = as.numeric(pred)),
                          aes(x = fp70_600m, y = inland_edge_dists_km, z = Occupancy)) +
  geom_raster(aes(fill = Occupancy),) + 
  geom_segment(x = 0, xend = 1, y = 190, yend = 190, linetype = "dashed") +
  geom_segment(x = 0, xend = 1, y = 210, yend = 210, linetype = "dashed") +
  geom_segment(x = 0, xend = 1, y = 590, yend = 590, linetype = "dashed") +
  geom_segment(x = 0, xend = 1, y = 610, yend = 610, linetype = "dashed") +
  geom_segment(x = 0, xend = 1, y = 990, yend = 990, linetype = "dashed") +
  geom_segment(x = 0, xend = 1, y = 1010, yend = 1010, linetype = "dashed") +
  geom_contour( colour = 'black') +
  xlab('Forest cover proportion') + 
  ylab('Distance to range edge (km)') +
  ggtitle("Poleward edges") +
  scale_y_continuous(breaks = c(0,500,1000,1500,2000,2500)) +
  scale_fill_gradient2(low = "white", mid = "royalblue",high = "darkblue",
                       limits = c(0,1), midpoint =0.5) + 
  theme_classic() +
 #geom_text_contour(skip = 0, nudge_y = 80, nudge_x = 0, size = 5, check_overlap = T, colour = 'black') +
  theme(axis.text = element_text(size = 16, colour = 'black'),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        legend.position = 'none')
poleward_model_plot
ggsave('figures/EquatorwardVsPoleward/poleward_model_plot.jpg')


