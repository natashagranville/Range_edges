#### Hard vs soft models ------------------

library(DHARMa) 
library(glmmTMB)
library(sjPlot)
library(performance)
library(piecewiseSEM) 

data <- read.csv('data/processed/Final_data.csv')

length(unique(data$Final_name))
length(unique(data$Study))

data_coast <- subset(data, sqrt_inland_edge_dists_km > sqrt_coastal_edge_dists_km)

nrow(data_coast) / nrow(data)

#### Hard edge model ---------------------------

hard_model <- glmmTMB(Occupancy ~ sqrt_coastal_edge_dists_km * fp70_600m + 
                    (1|Study) + (sqrt_coastal_edge_dists_km * fp70_600m | Final_name), 
                  data = data_coast, family = 'binomial')
save(hard_model, file = 'results/HardVsSoft/hard_model.Rdata')
summary(hard_model)

testResiduals(hard_model)
testQuantiles(hard_model)
testZeroInflation(hard_model)
check_collinearity(hard_model)

summary(hard_model)
rsquared(hard_model)

#### Soft egde model --------------------------
soft_model <- glmmTMB(Occupancy ~ sqrt_inland_edge_dists_km * fp70_600m + 
                    (1|Study) + (sqrt_inland_edge_dists_km * fp70_600m | Final_name), 
                  data = data_coast, family = 'binomial')
save(soft_model, file = 'results/HardVsSoft/soft_model.Rdata')
load('results/HardVsSoft/soft_model.Rdata')

testResiduals(soft_model)
testQuantiles(soft_model)
testZeroInflation(soft_model)
check_collinearity(soft_model)

summary(soft_model)
rsquared(soft_model)

AIC(soft_model, hard_model)

#### Supplementary table ----------------------
tab_model(hard_model, soft_model,
          show.intercept = F, show.se = T, show.ci = F, show.stat = T, # 'statistic' is the z value
          show.r2 = T, show.aic = T, show.icc = F,  show.re.var = FALSE, 
          transform = NULL, dv.labels = c("Hard edges", "Soft edges"),
          string.stat = "z", string.resp = "", digits = 2, digits.rsq = 2,
          pred.labels = c("Distance to hard range edge (square-root)", "Forest cover proportion",
                          "Interaction between distance to hard edge and forest cover",
                          "Distance to soft range edge (square-root)", 
                          "Interaction  between distance to soft edge and forest cover"),
          file = "results/HardVsSoft/HardVsSoft_model_table.html")

