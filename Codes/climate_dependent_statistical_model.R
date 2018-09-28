# run statistical models for mosquito abundances given climate-dependent mechanistic relationships for mosquito carrying capactiy
rm(list=ls()) #remove previous variable assignments

# load libraries 
library(MASS)
library(lmtest)
library(lme4)

# load data ------------------------------------------------
adults.site <- read.csv("Kenya/Concatenated_Data/vector/K_suitability_adults_site_lags.csv", head=T, stringsAsFactors = F)
adults.house <- read.csv("Kenya/Concatenated_Data/vector/K_suitability_adults_house_lags.csv", head=T, stringsAsFactors = F)
larvae.site <- read.csv("Kenya/Concatenated_Data/vector/K_suitability_larvae_site_lags.csv", head=T, stringsAsFactors = F)
larvae.house <- read.csv("Kenya/Concatenated_Data/vector/K_suitability_larvae_house_lags.csv", head=T, stringsAsFactors = F)
eggs.site <- read.csv("Kenya/Concatenated_Data/vector/K_suitability_eggs_site_lags.csv", head=T, stringsAsFactors = F)
eggs.house <- read.csv("Kenya/Concatenated_Data/vector/K_suitability_eggs_house_lags.csv", head=T, stringsAsFactors = F)

# set dates ------------------------------------------------
df.list <- list(adults.site, adults.house, larvae.site, larvae.house, eggs.site, eggs.house)
dataname <- c("adults.site", "adults.house", "larvae.site", "larvae.house", "eggs.site", "eggs.house")

for (i in 1:length(df.list)){
  df <- df.list[[i]]
  df[,"date_collected"] <- as.Date(df[,"date_collected"], "%Y-%m-%d")
  df[,"month"] <- as.numeric(format(df[,"date_collected"], "%m"))
  assign(dataname[i], df)
}
  
# split data into training and test datasets ---------------
# training data --
train.adults.site <- subset(adults.site, mn.yr <= "2016-08")
train.adults.house <- subset(adults.house, mn.yr <= "2016-08")

train.larvae.site <- subset(larvae.site, mn.yr <= "2016-12")
train.larvae.house <- subset(larvae.house, mn.yr <= "2016-12")

train.eggs.site <- subset(eggs.site, mn.yr <= "2015-12")
train.eggs.house <- subset(eggs.house, mn.yr <= "2015-12")

# testing data --
test.adults.site <- subset(adults.site, mn.yr >= "2017-09")
test.adults.house <- subset(adults.house, mn.yr >= "2017-09")

test.larvae.site <- subset(larvae.site, mn.yr >= "2017-08")
test.larvae.house <- subset(larvae.house, mn.yr >= "2017-08")

test.eggs.site <- subset(eggs.site, mn.yr >= "2016-05")
test.eggs.house <- subset(eggs.house, mn.yr >= "2016-05")

# center scale climate variables for analysis -------------- 
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

datasets <- list(train.adults.site, train.adults.house, train.larvae.site, train.larvae.house, train.eggs.site, train.eggs.house, test.adults.site, test.adults.house, test.larvae.site, test.larvae.house, test.eggs.site, test.eggs.house)
suitabilityMetrics <- c("T_suitability", "H_suitability", "R_suitability_briere", "R_suitability_hump", "R_suitability_linear")
datasetnames <- c("train.adults.site", "train.adults.house", "train.larvae.site", "train.larvae.house", "train.eggs.site", "train.eggs.house", "test.adults.site", "test.adults.house", "test.larvae.site", "test.larvae.house", "test.eggs.site", "test.eggs.house")

for (i in 1:length(datasets)){
  df <- datasets[[i]]
  scalevars <- df[,suitabilityMetrics] 
  scalevars[,] <- lapply(scalevars[,], center_scale)
  scalevars[,] <- lapply(scalevars[,], as.numeric)
  colnames(scalevars) <- paste0("scaled_", colnames(scalevars)) 
  tempDF <- cbind(df, scalevars)
  assign(datasetnames[i], tempDF)
}

# site level models -------------------------------------------------------------
siteDFs.train <- list(train.adults.site, train.eggs.site, train.larvae.site, train.larvae.site, train.larvae.site)
siteDFs.test <- list(test.adults.site, test.eggs.site, test.larvae.site, test.larvae.site, test.larvae.site)
siteaedesvar <- c(rep("prop.pos", 2), "prop_pos_early_instars", "prop_pos_late_instars", "prop_pos_pupae")
sitelagvar <- c("lag1prop", "lag1prop", "lag1ppEI", "lag1ppLI", "lag1ppPU")
sitedataname <- c("adults", "eggs", "early_instars", "late_instars", "pupae")

## add random effects models, figure out how to save data (j = 1:3 for rain)
# site_regressions <- data.frame(matrix(ncol = 4, nrow = 0))
# colnames(site_regressions) <- c("mosquito_stage", "rain_var", "model_r2", "predicted_r2")

for (i in 1:length(siteDFs.train)){
  trainData <- siteDFs.train[[i]]
  trainData$mosq <- trainData[,siteaedesvar[i]]
  trainData$lag <- trainData[,sitelagvar[i]]
  rain <- c("scaled_R_suitability_briere", "scaled_R_suitability_hump", "scaled_R_suitability_linear")
  for (j in 1:length(rain)){
    # lag model
    trainData$scaledR <- trainData[,rain[j]]
    lm.site <- lm(mosq ~ scaled_T_suitability + scaled_H_suitability + scaledR + lag + study_site, data=trainData)
    testData <- siteDFs.test[[i]]
    testData$mosq <- testData[,siteaedesvar[i]]
    testData$lag <- testData[,sitelagvar[i]]
    testData$scaledR <- testData[,rain[j]]
    testData$predYlm <- predict(lm.site, testData, type = "response") 
    site.pred.lm <- lm(testData$predY~testData$mosq)
    lm.fileName <- paste0("Kenya/Figures/K_functional_models/", sitedataname[i], "_", rain[j], "_site_lag.tiff")
    tiff(lm.fileName, width = 719, height = 483)
    plot(testData$mosq, testData$predYlm, xlim=c(0,1), ylim=c(0,1), pch=21, col='black', bg='blue', xlab=c("Observed mosquitoes (proportion positive)"), ylab=c("Predicted mosquitoes (proportion positive)"), main=paste0(dataname[i], "; site level (lag)"))
    abline(0,1)
    mtext(paste0("Model adjusted R^2 = ", round(summary(lm.site)$adj.r.squared, 2), "; Model predicted R^2 = ", round(summary(site.pred.lm)$adj.r.squared, 2)))
    dev.off()
    # random effects model
    glm.site <- glm(mosq ~ scaled_T_suitability + scaled_H_suitability + scaledR + study_site + (1|month), data=trainData)
    testData$predYglm <- predict.glm(glm.site, testData, type = "response") 
    site.pred.glm <- lm(testData$predYglm~testData$mosq)
    glm.fileName <- paste0("Kenya/Figures/K_functional_models/", sitedataname[i], "_", rain[j], "_site_ranef.tiff")
    tiff(glm.fileName, width = 719, height = 483)
    plot(testData$mosq, testData$predYglm, xlim=c(0,1), ylim=c(0,1), pch=21, col='black', bg='blue', xlab=c("Observed mosquitoes (proportion positive)"), ylab=c("Predicted mosquitoes (proportion positive)"), main=paste0(dataname[i], "; site level (ranef)"))
    abline(0,1)
    mtext(paste0("Model predicted R^2 = ", round(summary(site.pred.glm)$adj.r.squared, 2)))
    dev.off()
  }
}


# plot residuals
adult.site.pred <- lm(predY~prop.pos, data=test.adults.site)
lm.adults.site.res = resid(adult.site.pred)
plot(lm.adults.site.res, pch=21, col='black', bg='blue', ylab="Residuals")
abline(0,0)

# relative variable importance
varImp(lm.adults.site, scale = FALSE)

# comparison with mixed effects model with month as random effect
# library(lme4)
# glm.adults.site <- glm(prop.pos ~ scaled_T_suitability + scaled_H_suitability + scaled_R_suitability_briere + study_site + (1|month) , data = train.adults.site)
# test.adults.site$predY <- predict(glm.adults.site, test.adults.site)
# summary(glm.adults.site)

# house level models -------------------------------------------------------------
# data all seems to be zero-inflated
# library(car)
# yvar<-train.larvae.house$pupae_total+1
# qqp(yvar, "norm")
# qqp(yvar, "lnorm")
# nbinom <- fitdistr(yvar, "Negative Binomial")
# qqp(yvar, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
# poisson <- fitdistr(yvar, "Poisson")
# qqp(yvar, "pois", poisson$estimate)
# gamma <- fitdistr(yvar, "gamma")
# qqp(yvar, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

houseDFs.train <- list(train.adults.house, train.eggs.house, train.larvae.house, train.larvae.house, train.larvae.house)
houseDFs.test <- list(test.adults.house, test.eggs.house, test.larvae.house, test.larvae.house, test.larvae.house)
houseaedes <- c("aedes_total", "egg_total", "early_instar_total", "late_instar_total", "pupae_total")
houselagvar <- c("lag1total", "lag1total", "lag1EI", "lag1LI", "lag1PU")
housedataname <- c("adults", "eggs", "early_instars", "late_instars", "pupae")

for (i in 1:length(houseDFs.train)){
  trainData <- houseDFs.train[[i]]
  trainData$mosq <- trainData[,houseaedes[i]]
  trainData$lag <- trainData[,houselagvar[i]]
  rain <- c("scaled_R_suitability_briere", "scaled_R_suitability_hump", "scaled_R_suitability_linear")
  for (j in 1:length(rain)){
    trainData$scaledR <- trainData[,rain[j]]
    # lag model
    glmnb.house <- glm.nb(mosq ~ scaled_T_suitability + scaled_H_suitability + scaledR + house + lag, data = trainData)
    testData <- houseDFs.test[[i]]
    testData$mosq <- testData[,houseaedes[i]]
    testData$lag <- testData[,houselagvar[i]]
    testData$scaledR <- testData[,rain[j]]
    testData$predY <- predict(glmnb.house, testData, type = "response") 
    site.pred <- lm(testData$predY~testData[,houseaedes[i]])
    maxMosq <- max(testData$mosq, testData$predY, na.rm=T)
    fileName <- paste0("Kenya/Figures/K_functional_models/", housedataname[i], "_", rain[j], "_house_lag.tiff")
    tiff(fileName, width = 719, height = 483)
    plot(testData$mosq, testData$predY, pch=21, xlim=c(0,maxMosq), ylim=c(0,maxMosq), col='black', bg='blue', xlab=c("Total mosquitoes"), ylab=c("Predicted mosquitoes"), main=paste0(dataname[i], "; house level (lag)"))
    abline(0,1)
    mtext(paste0("Model predicted R^2 = ", round(summary(site.pred)$adj.r.squared, 2)))
    dev.off()
    # random effects model
    glmnb.house.re <- glm.nb(mosq ~ scaled_T_suitability + scaled_H_suitability + scaledR + house + (1|month), data = trainData)
    testData$predYre <- predict(glmnb.house.re, testData, type = "response") 
    site.pred.re <- lm(testData$predYre~testData$mosq)
    maxMosqRE <- max(testData$mosq, testData$predYre, na.rm=T)
    glm.fileName <- paste0("Kenya/Figures/K_functional_models/", sitedataname[i], "_", rain[j], "_house_ranef.tiff")
    tiff(glm.fileName, width = 719, height = 483)
    plot(testData$mosq, testData$predYre, pch=21, xlim=c(0,maxMosqRE), ylim=c(0,maxMosqRE), col='black', bg='blue', xlab=c("Total mosquitoes"), ylab=c("Predicted mosquitoes"), main=paste0(dataname[i], "; house level (ranef)"))
    abline(0,1)
    mtext(paste0("Model predicted R^2 = ", round(summary(site.pred.re)$adj.r.squared, 2)))
    dev.off()
    
  }
}

library(modEvA)
Dsquared(glmnb.house)


lm.site <- lm(prop_pos_pupae ~ scaled_T_suitability + scaled_H_suitability + scaled_R_suitability_hump + lag1ppPU + study_site, data=train.larvae.site)
summary(lm.site)
