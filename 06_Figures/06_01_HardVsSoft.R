#### Figure for hard vs soft -------------------


library(ggplot2)
library(glmmTMB)
library(dplyr)
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
  geom_sf(data = data_coast, size = 3) +
  theme_void()
coast_subset_map
ggsave('figures/HardVsSoft/coast_subset_map.jpg')


#### Explainer heatmap ----------------------------

##### Hard ---------------------------

pred_hard_dist_labels <- expand.grid(fp70_600m = c(0.1,0.5,0.9),
                           coastal_edge_dists_km = c(300,600,900))

pred_hard_dist_labels <- pred_hard_dist_labels %>% mutate(sqrt_coastal_edge_dists_km = sign(coastal_edge_dists_km) * sqrt(abs(coastal_edge_dists_km)))

pred_hard_dist_labels$predicted_occurrence <- predict(hard_model, pred_hard_dist_labels, type = "response", re.form = NA)

pred_hard_dist_labels$predicted_occurrence_rounded <- round(pred_hard_dist_labels$predicted_occurrence, 1)


pred_hard_dist_300 <- expand.grid(coastal_edge_dists_km = 300,
                          fp70_600m = seq(min(data$fp70_600m, na.rm = TRUE), max(data$fp70_600m, na.rm = T), length.out = 1000)) 
pred_hard_dist_300 <- pred_hard_dist_300 %>% mutate(sqrt_coastal_edge_dists_km = sign(coastal_edge_dists_km) * sqrt(abs(coastal_edge_dists_km)))
pred_hard_dist_300 <- mutate(pred_hard_dist_300, pred = unlist(predict(object = hard_model, newdata = pred_hard_dist_300, type = "response", re.form = NA)))


pred_hard_dist_600 <- expand.grid(coastal_edge_dists_km = 600,
                                  fp70_600m = seq(min(data$fp70_600m, na.rm = TRUE), max(data$fp70_600m, na.rm = T), length.out = 1000)) 
pred_hard_dist_600 <- pred_hard_dist_600 %>% mutate(sqrt_coastal_edge_dists_km = sign(coastal_edge_dists_km) * sqrt(abs(coastal_edge_dists_km)))
pred_hard_dist_600 <- mutate(pred_hard_dist_600, pred = unlist(predict(object = hard_model, newdata = pred_hard_dist_600, type = "response", re.form = NA)))


pred_hard_dist_900 <- expand.grid(coastal_edge_dists_km = 900,
                                  fp70_600m = seq(min(data$fp70_600m, na.rm = TRUE), max(data$fp70_600m, na.rm = T), length.out = 1000)) 
pred_hard_dist_900 <- pred_hard_dist_900 %>% mutate(sqrt_coastal_edge_dists_km = sign(coastal_edge_dists_km) * sqrt(abs(coastal_edge_dists_km)))
pred_hard_dist_900 <- mutate(pred_hard_dist_900, pred = unlist(predict(object = hard_model, newdata = pred_hard_dist_900, type = "response", re.form = NA)))

pred_hard_dist <- rbind(pred_hard_dist_300, pred_hard_dist_600, pred_hard_dist_900)


heatmap_explainer_hard_dist <- ggplot(data = pred_hard_dist, aes(x = fp70_600m, y = coastal_edge_dists_km)) +
  geom_point(aes(colour = pred), size = 15, shape = 15) +
  scale_colour_gradient2(low = "white", mid = "royalblue",high = "darkblue",
                       limits = c(0,1), midpoint = 0.5) + 
  theme_classic() +
  scale_y_continuous(limits = c(min(data_coast$coastal_edge_dists_km, na.rm = T), max(data_coast$coastal_edge_dists_km, na.rm = T)), 
                   breaks = c(300,600,900)) +
  geom_text(data = pred_hard_dist_labels, aes(x = fp70_600m, y = coastal_edge_dists_km, label = predicted_occurrence_rounded), size = 10) +
  labs(x = 'Forest cover proportion', y = 'Distance to range edge (km)', colour = 'Occupancy\nprobability') +
  theme(axis.title = element_text(size = 20), 
        legend.position = "none",
        axis.text = element_text(size = 20, colour = 'black'),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20))
heatmap_explainer_hard_dist
ggsave("figures/heatmap_explainer_hard_dist.jpg")

##### Soft ---------------------------


pred_soft_dist_labels <- expand.grid(fp70_600m = c(0.1,0.5,0.9),
                                     inland_edge_dists_km = c(200,600,1000))

pred_soft_dist_labels <- pred_soft_dist_labels %>% mutate(sqrt_inland_edge_dists_km = sign(inland_edge_dists_km) * sqrt(abs(inland_edge_dists_km)))

pred_soft_dist_labels$predicted_occurrence <- predict(soft_model, pred_soft_dist_labels, type = "response", re.form = NA)

pred_soft_dist_labels$predicted_occurrence_rounded <- round(pred_soft_dist_labels$predicted_occurrence, 1)


pred_soft_dist_200 <- expand.grid(inland_edge_dists_km = 200,
                                  fp70_600m = seq(min(data_coast$fp70_600m, na.rm = TRUE), max(data_coast$fp70_600m, na.rm = T), length.out = 1000)) 
pred_soft_dist_200 <- pred_soft_dist_200 %>% mutate(sqrt_inland_edge_dists_km = sign(inland_edge_dists_km) * sqrt(abs(inland_edge_dists_km)))
pred_soft_dist_200 <- mutate(pred_soft_dist_200, pred = unlist(predict(object = soft_model, newdata = pred_soft_dist_200, type = "response", re.form = NA)))


pred_soft_dist_600 <- expand.grid(inland_edge_dists_km = 600,
                                  fp70_600m = seq(min(data_coast$fp70_600m, na.rm = TRUE), max(data_coast$fp70_600m, na.rm = T), length.out = 1000)) 
pred_soft_dist_600 <- pred_soft_dist_600 %>% mutate(sqrt_inland_edge_dists_km = sign(inland_edge_dists_km) * sqrt(abs(inland_edge_dists_km)))
pred_soft_dist_600 <- mutate(pred_soft_dist_600, pred = unlist(predict(object = soft_model, newdata = pred_soft_dist_600, type = "response", re.form = NA)))


pred_soft_dist_1000 <- expand.grid(inland_edge_dists_km = 1000,
                                  fp70_600m = seq(min(data_coast$fp70_600m, na.rm = TRUE), max(data_coast$fp70_600m, na.rm = T), length.out = 1000)) 
pred_soft_dist_1000 <- pred_soft_dist_1000 %>% mutate(sqrt_inland_edge_dists_km = sign(inland_edge_dists_km) * sqrt(abs(inland_edge_dists_km)))
pred_soft_dist_1000 <- mutate(pred_soft_dist_1000, pred = unlist(predict(object = soft_model, newdata = pred_soft_dist_1000, type = "response", re.form = NA)))

pred_soft_dist <- rbind(pred_soft_dist_200, pred_soft_dist_600, pred_soft_dist_1000)


heatmap_explainer_soft_dist <- ggplot(data = pred_soft_dist, aes(x = fp70_600m, y = inland_edge_dists_km)) +
  geom_point(aes(colour = pred), size = 15, shape = 15) +
  scale_colour_gradient2(low = "white", mid = "royalblue",high = "darkblue",
                         limits = c(0,1), midpoint = 0.5) + 
  theme_classic() +
  scale_y_continuous(limits = c(min(data_coast$inland_edge_dists_km, na.rm = T), 1200), 
                     breaks = c(200,600,1000)) +
  geom_text(data = pred_soft_dist_labels, aes(x = fp70_600m, y = inland_edge_dists_km, label = predicted_occurrence_rounded), size = 10) +
  labs(x = 'Forest cover proportion', y = 'Distance to range edge (km)', colour = 'Occupancy\nprobability') +
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(size = 20, colour = 'black'),
        legend.position = "none",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20))
heatmap_explainer_soft_dist
ggsave("figures/heatmap_explainer_soft_dist.jpg")

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
  geom_segment(x = 0, xend = 1, y = 290, yend = 290, linetype = "dashed") +
  geom_segment(x = 0, xend = 1, y = 310, yend = 310, linetype = "dashed") +
  geom_segment(x = 0, xend = 1, y = 590, yend = 590, linetype = "dashed") +
  geom_segment(x = 0, xend = 1, y = 610, yend = 610, linetype = "dashed") +
  geom_segment(x = 0, xend = 1, y = 890, yend = 890, linetype = "dashed") +
  geom_segment(x = 0, xend = 1, y = 910, yend = 910, linetype = "dashed") +
  geom_contour( colour = 'black') +
  #geom_text_contour(skip = 0, nudge_y = 20, nudge_x = 0, size = 4, check_overlap = T, colour = 'black') +
  xlab('Forest cover proportion') + 
  ylab('Distance to range edge (km)') +
  ggtitle("Hard edges") +
  scale_fill_gradient2(low = "white", mid = "royalblue",high = "darkblue",
                       limits = c(0,1), midpoint = 0.5) + 
  theme_classic() +
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
  geom_segment(x = 0, xend = 1, y = 190, yend = 190, linetype = "dashed") +
  geom_segment(x = 0, xend = 1, y = 210, yend = 210, linetype = "dashed") +
  geom_segment(x = 0, xend = 1, y = 590, yend = 590, linetype = "dashed") +
  geom_segment(x = 0, xend = 1, y = 610, yend = 610, linetype = "dashed") +
  geom_segment(x = 0, xend = 1, y = 990, yend = 990, linetype = "dashed") +
  geom_segment(x = 0, xend = 1, y = 1010, yend = 1010, linetype = "dashed") +
  geom_contour( colour = 'black') +
  xlab('Forest cover proportion') + 
  ylab('Distance to range edge (km)') +
  ggtitle("Soft edges") +
  scale_fill_gradient2(low = "white", mid = "royalblue",high = "darkblue",
                       limits = c(0,1), midpoint =0.5) + 
  theme_classic() +
  #geom_text_contour(skip = 0, nudge_y = 80, nudge_x = 0, size = 5, check_overlap = T, colour = 'black') +
  theme(axis.text = element_text(size = 16, colour = 'black'),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        legend.position = 'none')
soft_model_plot
ggsave('figures/HardVsSoft/soft_model_plot.jpg')


