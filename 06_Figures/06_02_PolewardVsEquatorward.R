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
  geom_contour( colour = 'black') +
  xlab('Forest cover proportion') + 
  ylab('Distance to range edge (km)') +
  ggtitle("Poleward edges") +
  scale_y_continuous(breaks = c(0,500,1000,1500,2000,2500)) +
  scale_fill_gradient2(low = "white", mid = "royalblue",high = "darkblue",
                       limits = c(0,1), midpoint =0.5) + 
  theme_classic() +
 geom_text_contour(skip = 0, nudge_y = 80, nudge_x = 0, size = 5, check_overlap = T, colour = 'black') +
  theme(axis.text = element_text(size = 16, colour = 'black'),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        legend.position = 'none')
poleward_model_plot
ggsave('figures/EquatorwardVsPoleward/poleward_model_plot.jpg')


