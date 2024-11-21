#### Figure for forest species model ------------------

library(ggplot2)
library(glmmTMB)
library(metR)
library(sf)

behr <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

data <- read.csv('data/processed/Final_data.csv')

forest_species <- read.csv("C:/Users/tasha/OneDrive - Imperial College London/PhD/PROJECT_Spatial_patterns/data/raw/IUCN_ForestSpecies_Download/taxonomy.csv")
forest_species <- forest_species$scientificName
forest_species <- gsub(" ", "_", forest_species)

forest_data <- subset(data, Final_name %in% forest_species)



load("results/forest_model.Rdata")
summary(forest_model)

#### Heatmap --------------------------------------
pred <- expand.grid(fp70_600m = seq(min(forest_data$fp70_600m, na.rm = TRUE), max(forest_data$fp70_600m, na.rm = TRUE), length.out = 100),
                    inland_edge_dists_km = seq(-200, max(forest_data$inland_edge_dists_km, na.rm = TRUE), length.out = 100)) 

# square-root transformation for dist to edge
pred <- pred %>% mutate(sqrt_inland_edge_dists_km = sign(inland_edge_dists_km) * sqrt(abs(inland_edge_dists_km)))

# make predictions on this data
pred$Study <- NA
pred$Final_name <- NA

pred$pred <- predict(object = forest_model, newdata = pred, type = "response", re.form = NA)

forest_model_plot <- ggplot(mutate(pred, Occupancy = as.numeric(pred)),
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
forest_model_plot
ggsave('figures/Supplementary/forest_model_plot.jpg')
