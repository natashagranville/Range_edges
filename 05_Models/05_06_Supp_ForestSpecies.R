#### Forest species model ------------------------

library(DHARMa) 
library(glmmTMB)
library(sjPlot)
library(performance)
library(piecewiseSEM) 

behr <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

data <- read.csv('data/processed/Final_data.csv')

forest_species <- read.csv("C:/Users/tasha/OneDrive - Imperial College London/PhD/PROJECT_Spatial_patterns/data/raw/IUCN_ForestSpecies_Download/taxonomy.csv")
forest_species <- forest_species$scientificName
forest_species <- gsub(" ", "_", forest_species)

forest_data <- subset(data, Final_name %in% forest_species)
length(unique(forest_data$Final_name))
length(unique(data$Final_name))
length(unique(forest_data$Final_name)) / length(unique(data$Final_name)) * 100

forest_model <- glmmTMB(Occupancy ~ sqrt_inland_edge_dists_km * fp70_600m + 
                        (1|Study) + (sqrt_inland_edge_dists_km * fp70_600m | Final_name), 
                      data = forest_data, family = 'binomial')
save(forest_model, file = "results/supplementary_analyses/forest_model.Rdata")

testResiduals(forest_model)
testQuantiles(forest_model)
testZeroInflation(forest_model)
check_collinearity(forest_model)

summary(forest_model)
rsquared(forest_model)

tab_model(forest_model,
          show.intercept = F, show.se = T, show.ci = F, show.stat = T,
          show.r2 = T, show.aic = F, show.icc = F,  show.re.var = FALSE,
          transform = NULL, title = "forest model",
          string.stat = "z", string.resp = "", digits = 2, digits.rsq = 2,
          pred.labels = c("Distance to soft range edge (square-root)", "Forest cover proportion",
                          "Interaction between distance to soft edge and forest cover"),
          file = "results/supplementary_analyses/forest_model_table.html")
