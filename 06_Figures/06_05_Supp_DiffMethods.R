#### Supplementary analyses showing different ways of testing the equatorward vs poleward question --------------
library(dplyr)
library(glmmTMB)
library(ggplot2)
library(metR)

data <- read.csv('data/processed/Final_data.csv')


##### Three-way interaction model -----------------------------------------------
load('results/supplementary_analyses/interaction_model_pole_equator.Rdata')
summary(interaction_model_pole_equator)

equatorward_data <- subset(data, edge_direction == "equatorward_direction")
poleward_data <- subset(data, edge_direction == "poleward_direction")

equatorward_pred <- expand.grid(fp70_600m = seq(min(equatorward_data$fp70_600m, na.rm = TRUE), max(equatorward_data$fp70_600m, na.rm = TRUE), length.out = 100),
                    inland_edge_dists_km = seq(-200, max(equatorward_data$inland_edge_dists_km, na.rm = TRUE), length.out = 100)) 

# square-root transformation for dist to edge
equatorward_pred <- equatorward_pred %>% mutate(sqrt_inland_edge_dists_km = sign(inland_edge_dists_km) * sqrt(abs(inland_edge_dists_km)))

# make predictions on this data
equatorward_pred$Study <- NA
equatorward_pred$Final_name <- NA
equatorward_pred$edge_direction <- "equatorward_direction"

equatorward_pred <- mutate(equatorward_pred, pred = unlist(predict(object = interaction_model_pole_equator, newdata = equatorward_pred, type = "response", re.form = NA)))

equatorward_model_plot <- ggplot(mutate(equatorward_pred, Occupancy = as.numeric(pred)),
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
  geom_text_contour(skip = 0, nudge_y = 80, nudge_x = 0, size = 5, check_overlap = T, colour = 'black') +
  theme(axis.text = element_text(size = 16, colour = 'black'),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        legend.position = 'none')
equatorward_model_plot
ggsave("figures/Supplementary/interaction_model_plot_equatorward.jpg")




poleward_pred <- expand.grid(fp70_600m = seq(min(poleward_data$fp70_600m, na.rm = TRUE), max(poleward_data$fp70_600m, na.rm = TRUE), length.out = 100),
                                inland_edge_dists_km = seq(-200, max(poleward_data$inland_edge_dists_km, na.rm = TRUE), length.out = 100)) 

# square-root transformation for dist to edge
poleward_pred <- poleward_pred %>% mutate(sqrt_inland_edge_dists_km = sign(inland_edge_dists_km) * sqrt(abs(inland_edge_dists_km)))

# make predictions on this data
poleward_pred$Study <- NA
poleward_pred$Final_name <- NA
poleward_pred$edge_direction <- "poleward_direction"

poleward_pred <- mutate(poleward_pred, pred = unlist(predict(object = interaction_model_pole_equator, newdata = poleward_pred, type = "response", re.form = NA)))

poleward_model_plot <- ggplot(mutate(poleward_pred, Occupancy = as.numeric(pred)),
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
ggsave("figures/Supplementary/interaction_model_plot_poleward.jpg")



#### Latitude half models ------------------------------------------
load('results/supplementary_analyses/equatorward_half_model.Rdata')
load('results/supplementary_analyses/poleward_half_model.Rdata')


equatorward_half_data <- subset(data, latitude_half == "equatorward_half")
poleward_half_data <- subset(data, latitude_half == "poleward_half")


pred <- expand.grid(fp70_600m = seq(min(equatorward_half_data$fp70_600m, na.rm = TRUE), max(equatorward_half_data$fp70_600m, na.rm = TRUE), length.out = 100),
                    inland_edge_dists_km = seq(-200, max(equatorward_half_data$inland_edge_dists_km, na.rm = TRUE), length.out = 100)) 

# square-root transformation for dist to edge
pred <- pred %>% mutate(sqrt_inland_edge_dists_km = sign(inland_edge_dists_km) * sqrt(abs(inland_edge_dists_km)))

# make predictions on this data
pred$Study <- NA
pred$Final_name <- NA

pred <- mutate(pred, pred = unlist(predict(object = equatorward_half_model, newdata = pred, type = "response", re.form = NA)))

equatorward_half_model_plot <- ggplot(mutate(pred, Occupancy = as.numeric(pred)),
                                 aes(x = fp70_600m, y = inland_edge_dists_km, z = Occupancy)) +
  geom_raster(aes(fill = Occupancy),) + 
  geom_contour( colour = 'black') +
  xlab('Forest cover proportion') + 
  ylab('Distance to range edge (km)') +
  ggtitle("Equatorward half of range") +
  #scale_y_continuous(breaks = c(0,500,1000,1500,2000)) +
  scale_fill_gradient2(low = "white", mid = "royalblue",high = "darkblue",
                       limits = c(0,1), midpoint =0.5) + 
  theme_classic() +
  geom_text_contour(skip = 0, nudge_y = 80, nudge_x = 0, size = 5, check_overlap = T, colour = 'black') +
  theme(axis.text = element_text(size = 16, colour = 'black'),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        legend.position = 'none')
equatorward_half_model_plot
ggsave('figures/Supplementary/equatorward_model_plot.jpg')



pred <- expand.grid(fp70_600m = seq(min(poleward_half_data$fp70_600m, na.rm = TRUE), max(poleward_half_data$fp70_600m, na.rm = TRUE), length.out = 100),
                    inland_edge_dists_km = seq(-200, max(poleward_half_data$inland_edge_dists_km, na.rm = TRUE), length.out = 100)) 

# square-root transformation for dist to edge
pred <- pred %>% mutate(sqrt_inland_edge_dists_km = sign(inland_edge_dists_km) * sqrt(abs(inland_edge_dists_km)))

# make predictions on this data
pred$Study <- NA
pred$Final_name <- NA

pred <- mutate(pred, pred = unlist(predict(object = poleward_half_model, newdata = pred, type = "response", re.form = NA)))

poleward_half_model_plot <- ggplot(mutate(pred, Occupancy = as.numeric(pred)),
                                      aes(x = fp70_600m, y = inland_edge_dists_km, z = Occupancy)) +
  geom_raster(aes(fill = Occupancy),) + 
  geom_contour( colour = 'black') +
  xlab('Forest cover proportion') + 
  ylab('Distance to range edge (km)') +
  ggtitle("Poleward half of range") +
  scale_y_continuous(breaks = c(0,500,1000,1500,2000,2500)) +
  scale_fill_gradient2(low = "white", mid = "royalblue",high = "darkblue",
                       limits = c(0,1), midpoint =0.5) + 
  theme_classic() +
  geom_text_contour(skip = 0, nudge_y = 80, nudge_x = 0, size = 5, check_overlap = T, colour = 'black') +
  theme(axis.text = element_text(size = 16, colour = 'black'),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        legend.position = 'none')
poleward_half_model_plot
ggsave('figures/Supplementary/poleward_model_plot.jpg')




#### Direction and half --------------------------------------------------------

load('results/supplementary_analyses/equatorward_direction_equatorward_half_model.Rdata')

load('results/supplementary_analyses/poleward_direction_equatorward_half_model.Rdata')


data$direction_and_half <- paste(data$edge_direction, data$latitude_half, sep = "_")

equatorward_direction_equatorward_half_data <- subset(data, direction_and_half == "equatorward_direction_equatorward_half")

poleward_direction_poleward_half_data <- subset(data, direction_and_half == "poleward_direction_poleward_half")


pred <- expand.grid(fp70_600m = seq(min(equatorward_direction_equatorward_half_data$fp70_600m, na.rm = TRUE), max(equatorward_direction_equatorward_half_data$fp70_600m, na.rm = TRUE), length.out = 100),
                    inland_edge_dists_km = seq(-200, max(equatorward_direction_equatorward_half_data$inland_edge_dists_km, na.rm = TRUE), length.out = 100)) 

# square-root transformation for dist to edge
pred <- pred %>% mutate(sqrt_inland_edge_dists_km = sign(inland_edge_dists_km) * sqrt(abs(inland_edge_dists_km)))

# make predictions on this data
pred$Study <- NA
pred$Final_name <- NA

pred <- mutate(pred, pred = unlist(predict(object = equatorward_direction_equatorward_half_model, newdata = pred, type = "response", re.form = NA)))

equatorward_direction_half_model_plot <- ggplot(mutate(pred, Occupancy = as.numeric(pred)),
                                      aes(x = fp70_600m, y = inland_edge_dists_km, z = Occupancy)) +
  geom_raster(aes(fill = Occupancy),) + 
  geom_contour( colour = 'black') +
  xlab('Forest cover proportion') + 
  ylab('Distance to range edge (km)') +
  ggtitle("Equatorward-facing range edge\nin equatorward half of range") +
  #scale_y_continuous(breaks = c(0,500,1000,1500,2000)) +
  scale_fill_gradient2(low = "white", mid = "royalblue",high = "darkblue",
                       limits = c(0,1), midpoint =0.5) + 
  theme_classic() +
  geom_text_contour(skip = 0, nudge_y = 80, nudge_x = 0, size = 5, check_overlap = T, colour = 'black') +
  theme(axis.text = element_text(size = 16, colour = 'black'),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        legend.position = 'none')
equatorward_direction_half_model_plot
ggsave('figures/Supplementary/equatorward_direction_half_model_plot.jpg')



pred <- expand.grid(fp70_600m = seq(min(poleward_direction_poleward_half_data$fp70_600m, na.rm = TRUE), max(poleward_direction_poleward_half_data$fp70_600m, na.rm = TRUE), length.out = 100),
                    inland_edge_dists_km = seq(-200, max(poleward_direction_poleward_half_data$inland_edge_dists_km, na.rm = TRUE), length.out = 100)) 

# square-root transformation for dist to edge
pred <- pred %>% mutate(sqrt_inland_edge_dists_km = sign(inland_edge_dists_km) * sqrt(abs(inland_edge_dists_km)))

# make predictions on this data
pred$Study <- NA
pred$Final_name <- NA

pred <- mutate(pred, pred = unlist(predict(object = poleward_direction_poleward_half_model, newdata = pred, type = "response", re.form = NA)))

poleward_half_direction_model_plot <- ggplot(mutate(pred, Occupancy = as.numeric(pred)),
                                   aes(x = fp70_600m, y = inland_edge_dists_km, z = Occupancy)) +
  geom_raster(aes(fill = Occupancy),) + 
  geom_contour( colour = 'black') +
  xlab('Forest cover proportion') + 
  ylab('Distance to range edge (km)') +
  ggtitle("Poleward facing range edge\nin poleward half of range") +
  scale_y_continuous(breaks = c(0,500,1000,1500,2000,2500)) +
  scale_fill_gradient2(low = "white", mid = "royalblue",high = "darkblue",
                       limits = c(0,1), midpoint =0.5) + 
  theme_classic() +
  geom_text_contour(skip = 0, nudge_y = 80, nudge_x = 0, size = 5, check_overlap = T, colour = 'black') +
  theme(axis.text = element_text(size = 16, colour = 'black'),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        legend.position = 'none')
poleward_half_direction_model_plot
ggsave('figures/Supplementary/poleward_half_direction_model_plot.jpg')
