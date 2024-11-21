#### Supplementary analyses showing different ways of testing the equatorward vs poleward question --------------

library(DHARMa) 
library(dplyr)
library(glmmTMB)
library(sjPlot)
library(piecewiseSEM) 

data <- read.csv('data/processed/Final_data.csv')


##### Three-way interaction model -----------------------------------------------
interaction_model_pole_equator <- glmmTMB(Occupancy ~ sqrt_inland_edge_dists_km * fp70_600m * edge_direction + 
                                         (1|Study) + (sqrt_inland_edge_dists_km * fp70_600m * edge_direction | Final_name), 
                                       data = data, family = 'binomial')
save(interaction_model_pole_equator, file = 'results/supplementary_analyses/interaction_model_pole_equator.Rdata')
summary(interaction_model_pole_equator)

tab_model(interaction_model_pole_equator,
          show.intercept = F, show.se = T, show.ci = F, show.stat = T, # 'statistic' is the z value
          show.r2 = T, show.aic = T, show.icc = F,  show.re.var = FALSE, 
          transform = NULL,
          string.stat = "z", string.resp = "", digits = 2, digits.rsq = 2,
          file = "results/supplementary_analyses/interaction_model_pole_equator.html")

interaction_model_pole_equator_releveled <- glmmTMB(Occupancy ~ sqrt_inland_edge_dists_km * fp70_600m * relevel(as.factor(edge_direction), ref = 'poleward_direction')  + 
                                            (1|Study) + (sqrt_inland_edge_dists_km * fp70_600m * relevel(as.factor(edge_direction), ref = 'poleward_direction')  | Final_name), 
                                          data = data, family = 'binomial')
summary(interaction_model_pole_equator_releveled)

##### Latitudinal half ---------------------------------------------------------
data$direction_and_half <- paste(data$edge_direction, data$latitude_half, sep = "_")

equatorward_half_data <- subset(data, latitude_half == "equatorward_half")
poleward_half_data <- subset(data, latitude_half == "poleward_half")

equatorward_direction_data <- subset(data, edge_direction == "equatorward_direction")
poleward_direction_data <- subset(data, edge_direction == "poleward_direction")

equatorward_direction_equatorward_half_data <- subset(equatorward_direction_data, direction_and_half == "equatorward_direction_equatorward_half")
poleward_direction_poleward_half_data <- subset(poleward_direction_data, direction_and_half == "poleward_direction_poleward_half")


nrow(equatorward_direction_equatorward_half_data) / nrow(equatorward_direction_data)
nrow(poleward_direction_poleward_half_data) / nrow(poleward_direction_data)



equatorward_half_model <- glmmTMB(Occupancy ~ sqrt_inland_edge_dists_km * fp70_600m + 
                                                          (1|Study) + (sqrt_inland_edge_dists_km * fp70_600m | Final_name), 
                                                        data = equatorward_half_data, family = 'binomial')
save(equatorward_half_model, file = 'results/supplementary_analyses/equatorward_half_model.Rdata')
summary(equatorward_half_model)


poleward_half_data <- subset(data, latitude_half == "poleward_half")

poleward_half_model <- glmmTMB(Occupancy ~ sqrt_inland_edge_dists_km * fp70_600m + 
                                    (1|Study) + (sqrt_inland_edge_dists_km * fp70_600m | Final_name), 
                                  data = poleward_half_data, family = 'binomial')
save(poleward_half_model, file = 'results/supplementary_analyses/poleward_half_model.Rdata')
summary(poleward_half_model)



tab_model(equatorward_half_model, poleward_half_model,
          show.intercept = F, show.se = T, show.ci = F, show.stat = T, # 'statistic' is the z value
          show.r2 = T, show.aic = T, show.icc = F,  show.re.var = FALSE, 
          transform = NULL,
          string.stat = "z", string.resp = "", digits = 2, digits.rsq = 2,
          file = "results/supplementary_analyses/latitude_half_model.html")



##### Latitudinal half and edge direction --------------------------------------
data$direction_and_half <- paste(data$edge_direction, data$latitude_half, sep = "_")

data %>% group_by(direction_and_half) %>%
  summarise(n())

equatorward_direction_equatorward_half_data <- subset(data, direction_and_half == "equatorward_direction_equatorward_half")

equatorward_direction_equatorward_half_model <- glmmTMB(Occupancy ~ sqrt_inland_edge_dists_km * fp70_600m + 
                                   (1|Study) + (sqrt_inland_edge_dists_km * fp70_600m | Final_name), 
                                 data = equatorward_direction_equatorward_half_data, family = 'binomial')
save(equatorward_direction_equatorward_half_model, file = 'results/supplementary_analyses/equatorward_direction_equatorward_half_model.Rdata')
summary(equatorward_direction_equatorward_half_model)

poleward_direction_poleward_half_data <- subset(data, direction_and_half == "poleward_direction_poleward_half")
poleward_direction_poleward_half_model <- glmmTMB(Occupancy ~ sqrt_inland_edge_dists_km * fp70_600m + 
                                                          (1|Study) + (sqrt_inland_edge_dists_km * fp70_600m | Final_name), 
                                                        data = poleward_direction_poleward_half_data, family = 'binomial')
save(poleward_direction_poleward_half_model, file = 'results/supplementary_analyses/poleward_direction_poleward_half_model.Rdata')
summary(poleward_direction_poleward_half_model)


tab_model(equatorward_direction_equatorward_half_model, poleward_direction_poleward_half_model,
          show.intercept = F, show.se = T, show.ci = F, show.stat = T, # 'statistic' is the z value
          show.r2 = T, show.aic = F, show.icc = F,  show.re.var = FALSE, 
          transform = NULL,
          string.stat = "z", string.resp = "", digits = 2, digits.rsq = 2,
          file = "results/supplementary_analyses/edge_direction_latitude_half_model.html")
