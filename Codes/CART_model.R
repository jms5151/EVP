# CART model to compare conditions where model simulations under/over predict vector data ------------
rm(list=ls()) #remove previous variable assignments

# load libraries -------------------------------------------------------------------------------------
library(rpart)
library(tree)
library(partykit)

# load data ------------------------------------------------------------------------------------------
# source("C:/Users/Jamie/Box Sync/DENV/Codes/concat_zscore_climate_data_for_CART.R")
zscoreDF <- read.csv("Kenya/Concatenated_Data/zscores_with_climate_data.csv", head=T)
test <- subset(zscoreDF, aedes_total_class != "Comparable")

tree.model <- tree(aedes_total_class ~ cum_R_mosq + var_T_mosq, data=zscoreDF)
tree.model
summary(tree.model)
plot(tree.model)
text(tree.model)

rpart.tree <- rpart(aedes_total_class ~ mean_T_mosq + var_T_mosq + mean_H_mosq + var_H_mosq +
                    cum_R_mosq + var_R_mosq + study_site, data=zscoreDF)
plot(rpart.tree, uniform=TRUE, branch=0.6, margin=0.05)
text(rpart.tree, all=TRUE, use.n=TRUE)
# title("Training Set's Classification Tree")

prune.rpart.tree <- prune(rpart.tree, cp=0.02) # pruning the tree
plot(prune.rpart.tree, uniform=TRUE, branch=0.6)
text(prune.rpart.tree, all=TRUE, use.n=TRUE)

rparty.tree <- as.party(rpart.tree)
# tiff("test.tiff")
plot(rparty.tree)
# dev.off()
# par(mar=c(4,4,4,4))
