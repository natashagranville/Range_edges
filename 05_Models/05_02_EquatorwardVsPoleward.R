#### Equatorward vs poleward models ------------------

library(DHARMa) 
library(glmmTMB)
library(sjPlot)
library(performance)
library(piecewiseSEM) 

behr <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

data <- read.csv('data/processed/Final_data.csv')

equatorward_data <- subset(data, edge_direction == "equatorward_direction")
poleward_data <- subset(data, edge_direction == "poleward_direction")

not_detectable <- data[is.na(data$edge_direction),]

nrow(equatorward_data)/ nrow(data) * 100
nrow(poleward_data)/ nrow(data) * 100
nrow(not_detectable)/ nrow(data) * 100


#### Equatorward model ------------------------

equatorward_model <- glmmTMB(Occupancy ~ sqrt_inland_edge_dists_km * fp70_600m + 
                               (1|Study) + (sqrt_inland_edge_dists_km * fp70_600m | Final_name), 
                             data = equatorward_data, family = 'binomial')
save(equatorward_model, file = "results/EquatorwardVsPoleward/equatorward_model.Rdata")

testResiduals(equatorward_model)
testQuantiles(equatorward_model)
testZeroInflation(equatorward_model)
check_collinearity(equatorward_model)

summary(equatorward_model)
rsquared(equatorward_model)


#### Poleward model --------------------------


poleward_model <- glmmTMB(Occupancy ~ sqrt_inland_edge_dists_km * fp70_600m + 
                               (1|Study) + (sqrt_inland_edge_dists_km * fp70_600m | Final_name), 
                             data = poleward_data, family = 'binomial')
save(poleward_model, file = "results/EquatorwardVsPoleward/poleward_model.Rdata")

testResiduals(poleward_model)
testQuantiles(poleward_model)
testZeroInflation(poleward_model)
check_collinearity(poleward_model)

summary(poleward_model)
rsquared(poleward_model)


#### Supplementary table ----------------------
tab_model(equatorward_model, poleward_model,
          show.intercept = F, show.se = T, show.ci = F, show.stat = T, # 'statistic' is the z value
          show.r2 = T, show.aic = F, show.icc = F,  show.re.var = FALSE, 
          transform = NULL, dv.labels = c("Equatorward edges", "Poleward edges"),
          string.stat = "z", string.resp = "", digits = 2, digits.rsq = 2,
          pred.labels = c("Distance to soft range edge (square-root)", "Forest cover proportion",
                          "Interaction between distance to soft edge and forest cover"),
          file = "results/EquatorwardVsPoleward/EquatorwardVsPoleward_model_table.html")


