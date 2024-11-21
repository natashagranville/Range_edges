#### Controlling for range size ---------------------------------------

library(dplyr)
library(glmmTMB)
library(sjPlot)
library(DHARMa)
library(performance)
library(units)
library(sf)

data <- read.csv('data/processed/Final_data.csv')

ranges <- st_read("data/processed/Cleaned_BirdLife_ranges_Americas/Cleaned_BirdLife_ranges_Americas.shp")

species_list <- unique(data$Final_name)

range_areas <- data.frame(Final_name = character(), area = numeric())

for (i in 1:length(species_list)) {
  
  tryCatch({
    
    species <- species_list[i]
    
    species_range <- subset(ranges, sci_nam == species)
    
    area_value <- set_units(st_area(species_range), "km^2")
    
    range_area <- data.frame(Final_name = species, area = as.numeric(area_value))
    
    range_areas <- rbind(range_areas, range_area)
    
    
  }, error = function(e) {
    print(paste("Error:", conditionMessage(e), "\n"))
  })
  
}

data <- left_join(data, range_areas[,c('Final_name',  'area')], by = 'Final_name')


#data <- na.omit(data)

model_with_range_size <- glmmTMB(Occupancy ~ sqrt_inland_edge_dists_km * fp70_600m + log(area) + 
                        (1|Study) + (sqrt_inland_edge_dists_km * fp70_600m | Final_name), 
                      data = data, family = 'binomial')
piecewiseSEM::rsquared(model_with_range_size)
summary(model_with_range_size)
check_collinearity(model_with_range_size)
testResiduals(model_with_range_size)
testQuantiles(model_with_range_size)

save(model_with_range_size, file = "results/supplementary_analyses/model_with_range_size.Rdata")

tab_model(model_with_range_size,
          show.intercept = F, show.se = T, show.ci = F, show.stat = T,
          show.r2 = T, show.aic = F, show.icc = F,  show.re.var = FALSE,
          transform = NULL, 
          string.stat = "z", string.resp = "", digits = 2, digits.rsq = 2,
          pred.labels = c("Distance to soft range edge (square-root)", "Forest cover proportion",
                          "Range size (log-transformed)",
                          "Interaction between distance to soft edge and forest cover"),
          file = "results/supplementary_analyses/model_with_range_size.html")
