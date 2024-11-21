#### Controlling for range size ---------------------------------------

library(dplyr)
library(cowplot)
library(glmmTMB)
library(ggplot2)
library(metR)

data <- read.csv('data/processed/Final_data.csv')

AVONET <- read.csv("C:/Users/tasha/OneDrive - Imperial College London/PhD/PROJECT_Spatial_patterns/data/raw/AVONET_data/TraitData/AVONET1_BirdLife.csv")

AVONET$Final_name <- gsub(" ", "_", AVONET$Species1)

data <- left_join(data, AVONET[,c('Final_name',  'Range.Size')], by = 'Final_name')


data <- na.omit(data)


load("results/supplementary_analyses/model_with_range_size.Rdata")

plot_heatmap <- function(range_size, title){
  
  range_data <- subset(data, area < range_size)
  
  pred <- expand.grid(fp70_600m = seq(min(range_data$fp70_600m, na.rm = TRUE), max(range_data$fp70_600m, na.rm = TRUE), length.out = 100),
                      inland_edge_dists_km = seq(-200, max(range_data$inland_edge_dists_km, na.rm = TRUE), length.out = 100)) 
  pred$area <- rep(range_size, 10000)
  
  # square-root transformation for dist to edge
  pred <- pred %>% mutate(sqrt_inland_edge_dists_km = sign(inland_edge_dists_km) * sqrt(abs(inland_edge_dists_km)))
  
  # make predictions on this data
  pred$Study <- NA
  pred$Final_name <- NA
  
  pred <- mutate(pred, pred = unlist(predict(object = model_with_range_size, newdata = pred, type = "response", re.form = NA)))
  
  small_range_plot <- ggplot(mutate(pred, Occupancy = as.numeric(pred)),
                             aes(x = fp70_600m, y = inland_edge_dists_km, z = Occupancy)) +
    geom_raster(aes(fill = Occupancy),) + 
    geom_contour( colour = 'black') +
    xlab('Forest cover proportion') + 
    ylab('Distance to range edge (km)') +
    ggtitle(title) +
    scale_fill_gradient2(low = "white", mid = "royalblue",high = "darkblue",
                         limits = c(0,1), midpoint =0.5) + 
    theme_classic() +
    geom_text_contour(skip = 0, nudge_y = 60, nudge_x = 0, size = 5, check_overlap = T, colour = 'black') +
    theme(axis.text = element_text(size = 16, colour = 'black'),
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 18),
          legend.position = 'none')
  return(small_range_plot)
  
  
}


plot_grid(
  plot_heatmap(summary(data$area)[2], "25th percentile of\nrange sizes"),
  plot_heatmap(summary(data$area)[3], "median of\nrange sizes"),
  plot_heatmap(summary(data$area)[4], "75th percentile of\nrange sizes"),
  nrow = 1  
)

ggsave("figures/Supplementary/range_size_additive.jpg")
