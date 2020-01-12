# CART model to compare conditions where model simulations under/over predict vector data ------------
rm(list=ls()) #remove previous variable assignments

# load libraries -------------------------------------------------------------------------------------
library(rpart)
library(rattle)

# load and format data -------------------------------------------------------------------------------
load("Concatenated_Data/zscores_with_climate_data_aedes.RData")
zscoreSub_aedes$Country <- ifelse(zscoreSub_aedes$Site=="Chulaimbo"|zscoreSub_aedes$Site=="Kisumu"|zscoreSub_aedes$Site=="Msambweni"|zscoreSub_aedes$Site=="Ukunda", "Kenya", "Ecuador")
zscoreSub_aedes$Urbanization <- ifelse(zscoreSub_aedes$Site=="Chulaimbo"|zscoreSub_aedes$Site=="Msambweni"|zscoreSub_aedes$Site=="Zaruma"|zscoreSub_aedes$Site=="Portovelo", "Rural", "Urban")
zscoreSub_aedes$CoastalInland <- ifelse(zscoreSub_aedes$Site=="Chulaimbo"|zscoreSub_aedes$Site=="Kisumu"|zscoreSub_aedes$Site=="Zaruma"|zscoreSub_aedes$Site=="Portovelo"|zscoreSub_aedes$Site=="Huaquillas", "Inland", "Coastal")

load("Concatenated_Data/zscores_with_climate_data_dengue.RData")
zscoreSub_dengue$Country <- ifelse(zscoreSub_dengue$Site=="Chulaimbo"|zscoreSub_dengue$Site=="Kisumu"|zscoreSub_dengue$Site=="Msambweni"|zscoreSub_dengue$Site=="Ukunda", "Kenya", "Ecuador")
zscoreSub_dengue$Urbanization <- ifelse(zscoreSub_dengue$Site=="Chulaimbo"|zscoreSub_dengue$Site=="Msambweni"|zscoreSub_dengue$Site=="Zaruma"|zscoreSub_dengue$Site=="Portovelo", "Rural", "Urban")
zscoreSub_dengue$CoastalInland <- ifelse(zscoreSub_dengue$Site=="Chulaimbo"|zscoreSub_dengue$Site=="Kisumu"|zscoreSub_dengue$Site=="Zaruma"|zscoreSub_dengue$Site=="Portovelo"|zscoreSub_dengue$Site=="Huaquillas", "Inland", "Coastal")

# mosquitoes
rpart.tree.adults <- rpart(Adult_correspondence_magnitude ~ T_min + T_mean + T_max + T_var + H_min + H_mean + H_max + H_var + R_min + R_mean + R_max + R_var + Site + Country + Urbanization + CoastalInland, data=zscoreSub_aedes)
fancyRpartPlot(rpart.tree.adults)
prune.rpart.tree.adults <- prune(rpart.tree.adults, cp=0.04) # pruning the tree
fancyRpartPlot(prune.rpart.tree.adults)

# dengue
rpart.tree.dengue <- rpart(Dengue_correspondence_magnitude ~ T_min + T_mean + T_max + T_var + H_min + H_mean + H_max + H_var + R_min + R_mean + R_max + R_var + Country + Urbanization + CoastalInland, data=zscoreSub_dengue)
fancyRpartPlot(rpart.tree.dengue)
prune.rpart.tree.dengue <- prune(rpart.tree.dengue, cp=0.03) # pruning the tree
fancyRpartPlot(prune.rpart.tree.dengue)

