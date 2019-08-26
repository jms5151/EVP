# assessing different rainfall functions in models with data ------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(plyr)

cases <- read.csv("Concatenated_Data/case_data/merged_case_data.csv", head=T, stringsAsFactors = F)

# load vector data
vectors <- read.csv("Concatenated_Data/vector_data/merged_vector_data.csv", head=T, stringsAsFactors = F)

# merge observed data
data <- merge(cases, vectors, by=c("Site", "Date", "Year.Month"), all=T)
data$Country <- ifelse(data$Site=="Chulaimbo"|data$Site=="Kisumu"|data$Site=="Msambweni"|data$Site=="Ukunda", "Kenya", "Ecuador")

# load modeled data with different rainfall functions
models <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_THR_diff_rain_functions.csv", head=T, stringsAsFactors = F)
models$Mtot <- models$M1 + models$M2 + models$M3
models$Country <- ifelse(models$Site=="Chulaimbo"|models$Site=="Kisumu"|models$Site=="Msambweni"|models$Site=="Ukunda", "Kenya", "Ecuador")
models$R_function2 <- paste(models$R_function, models$Rain_metric, models$Rmax, sep="_")

# function
tau.calc <- function(x, y) {
  t <- cor.test(x, y, method="kendall")
  unname(t$estimate)
}

# set up dataframe
corr.rainfall <- data.frame(matrix(ncol=7, nrow=0))
colnames(corr.rainfall) <- c("Region", "Yvar", "tau", "Rain_metric", "R_function", "Rmax", "R_Name")

regionnames <- c("Kenya", "Ecuador")
rfunctions <- unique(models$R_function2)

for (m in 1:length(regionnames)){
  datasub <- subset(data, Country == regionnames[m])
  modelsub <- subset(models, Country == regionnames[m])
  rainDF <- merge(datasub, modelsub, by=c("Country", "Date", "Site"), all=T)
  if (regionnames[m]=="Ecuador"){
    yVars <- c("denv_positive", "denv_positive_clinically_diagnosed", "chikv_positive", "aedes_total")
  } else {
    yVars <- c("denv_positive", "chikv_positive", "aedes_total", "aedes_total_bg", "pupae_total", "late_instar_total", "early_instar_total", "egg_total")
  }  
  for (n in 1:length(yVars)){
    firstLetter <- substr(yVars[n],1,1)
    if (firstLetter == "d"|firstLetter == "c"){
      modVar <- "I"
    } else {
      modVar <- "Mtot"
    }
    for (o in 1:length(rfunctions)){
      tempRainDF <- subset(rainDF, R_function2 == rfunctions[o])
      Rain_metric <- unique(tempRainDF$Rain_metric)
      R_function <- unique(tempRainDF$R_function)
      Rmax <- unique(tempRainDF$Rmax)
      tempRainDF <- tempRainDF[,c(yVars[n], modVar)]
      tempRainDF <- tempRainDF[complete.cases(tempRainDF),]
      if (nrow(tempRainDF) == 0){
        tau <- NA
      } else {
        tau <- round(tau.calc(tempRainDF[,yVars[n]], tempRainDF[,modVar]), 2)
      }
      tmpvec <- c(regionnames[m], yVars[n], tau, Rain_metric, R_function, Rmax, rfunctions[o])
      corr.rainfall[nrow(corr.rainfall)+1,] <- tmpvec
    }
  }
}

write.csv(corr.rainfall, "Concatenated_Data/model_assessment/tau_diff_rain_functions.csv", row.names=F)
corr.rainfall <- read.csv("Concatenated_Data/model_assessment/tau_diff_rain_functions.csv", head=T)

library(tidyverse)
best <- corr.rainfall %>% 
  group_by(Region, Yvar)  %>%  
  filter(tau == max(tau, na.rm=T))

library(ggplot2)
par(mar=c(5.1,10,4.1,2.1))
df <- subset(corr.rainfall, Yvar == "denv_positive" & Region == "Ecuador")
dotchart(as.numeric(best$tau), labels=best$Yvar, cex=1.2, groups= as.factor(best$Region)
         , xlab="Tau", pt.cex=2, gcolor="black", pch = 16)

