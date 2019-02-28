# CART model to compare conditions where model simulations under/over predict vector data ------------
rm(list=ls()) #remove previous variable assignments

# load libraries -------------------------------------------------------------------------------------
library(rpart)
library(rattle)

# load data ------------------------------------------------------------------------------------------
# source("C:/Users/Jamie/Box Sync/DENV/Codes/concat_zscore_climate_data_for_CART.R")
zscoreDF <- read.csv("Concatenated_Data/zscores_with_climate_data.csv", head=T)
zscoreDF$Country <- ifelse(zscoreDF$Site=="Chulaimbo"|zscoreDF$Site=="Kisumu"|zscoreDF$Site=="Msambweni"|zscoreDF$Site=="Ukunda", "Kenya", "Ecuador")

# mosquitoes
rpart.tree.adults <- rpart(Adult_correspondence_magnitude ~ T_min + T_mean + T_max + T_var + H_min + H_mean + H_max + H_var + R_min + R_mean + R_max + R_var + R_cum + R_sum_days + Site + Country, data=zscoreDF)
fancyRpartPlot(rpart.tree.adults)
prune.rpart.tree.adults <- prune(rpart.tree.adults, cp=0.04) # pruning the tree
fancyRpartPlot(prune.rpart.tree.adults)

# dengue
rpart.tree.dengue <- rpart(Dengue_correspondence_magnitude ~ T_min + T_mean + T_max + T_var + H_min + H_mean + H_max + H_var + R_min + R_mean + R_max + R_var + R_cum + R_sum_days + Site + Country, data=zscoreDF)
fancyRpartPlot(rpart.tree.dengue)
prune.rpart.tree.dengue <- prune(rpart.tree.dengue, cp=0.04) # pruning the tree
fancyRpartPlot(prune.rpart.tree.dengue)
