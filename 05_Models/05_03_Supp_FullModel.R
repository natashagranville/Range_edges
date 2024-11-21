#### Full model ------------------------

library(DHARMa) 
library(glmmTMB)
library(sjPlot)
library(performance)
library(piecewiseSEM) 

behr <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

data <- read.csv('data/processed/Final_data.csv')

full_model <- glmmTMB(Occupancy ~ sqrt_inland_edge_dists_km * fp70_600m + 
                        (1|Study) + (sqrt_inland_edge_dists_km * fp70_600m | Final_name), 
                      data = data, family = 'binomial')
save(full_model, file = "results/supplementary_analyses/full_model.Rdata")

testResiduals(full_model)
testQuantiles(full_model)
testZeroInflation(full_model)
check_collinearity(full_model)

summary(full_model)
rsquared(full_model)

tab_model(full_model,
          show.intercept = F, show.se = T, show.ci = F, show.stat = T,
          show.r2 = T, show.aic = F, show.icc = F,  show.re.var = FALSE,
          transform = NULL, title = "Full model",
          string.stat = "z", string.resp = "", digits = 2, digits.rsq = 2,
          pred.labels = c("Distance to soft range edge (square-root)", "Forest cover proportion",
                          "Interaction between distance to soft edge and forest cover"),
          file = "results/supplementary_analyses/full_model_table.html")
