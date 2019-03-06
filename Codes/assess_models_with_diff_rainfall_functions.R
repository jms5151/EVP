# assessing different rainfall functions in models with data ------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(plyr)

cases <- read.csv("Concatenated_Data/case_data/merged_case_data.csv", head=T, stringsAsFactors = F)

# load vector data
vectors <- read.csv("Concatenated_Data/vector_data/merged_vector_data.csv", head=T, stringsAsFactors = F)

# merge observed data
data <- merge(cases, vectors, by=c("Site", "Date"), all=T)
data$Country <- ifelse(data$Site=="Chulaimbo"|data$Site=="Kisumu"|data$Site=="Msambweni"|data$Site=="Ukunda", "Kenya", "Ecuador")

# load modeled data with different rainfall functions
models <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_THR_diff_rain_functions.csv", head=T, stringsAsFactors = F)
models$Mtot <- models$M1 + models$M2 + models$M3
models$Country <- ifelse(models$Site=="Chulaimbo"|models$Site=="Kisumu"|models$Site=="Msambweni"|models$Site=="Ukunda", "Kenya", "Ecuador")

# function
tau.calc <- function(x, y) {
  t <- cor.test(x, y, method="kendall")
  unname(t$estimate)
}

# set up dataframe
corr.rainfall <- data.frame(matrix(ncol=12, nrow=0))
colnames(corr.rainfall) <- c("Region", "Yvar", "tau_cum_exp_de", "tau_cum_exp_in", "tau_cum_quadratic", "tau_cum_rskewed", "tau_cum_briere", "tau_days_exp_de", "tau_days_exp_in", "tau_days_qudratic", "tau_days_rskewed", "tau_days_briere")

regionnames <- c("Kenya", "Ecuador")
rainmetrics <- unique(models$Rain_metric)
rfunctions <- unique(models$R_function)

for (m in 1:length(regionnames)){
  datasub <- subset(data, Country == regionnames[m])
  modelsub <- subset(models, Country == regionnames[m])
  rainDF <- merge(datasub, modelsub, by=c("Country", "Date", "Site"), all=T)
  if (regionnames[m]=="Ecuador"){
    yVars <- c("denv_positive_weekly_any", "denv_positive_0311_weekly", "denv_positive_1418_weekly", "denv_positive_weekly", "chikv_positive_weekly", "aedes_total_weekly")
  } else {
    yVars <- c("denv_positive_weekly", "chikv_positive_weekly", "aedes_total_weekly", "pupae_total_weekly", "late_instar_total_weekly", "early_instar_total_weekly", "egg_total_weekly")
  }  
  for (n in 1:length(yVars)){
    firstLetter <- substr(yVars[n],1,1)
    if (firstLetter == "d"|firstLetter == "c"){
      modVar <- "I"
    } else {
      modVar <- "Mtot"
    }
    tmpvec <- c()
    for (o in 1:length(rainmetrics)){
      for (p in 1:length(rfunctions)){
        tempRainDF <- subset(rainDF, Rain_metric == rainmetrics[o])
        tempRainDF <- subset(tempRainDF, rfunctions == R_function[p])
        tempRainDF <- tempRainDF[c(yVars[n], modVar)]
        tempRainDF <- tempRainDF[complete.cases(tempRainDF),]
        tau <- tau.calc(tempRainDF[,yVars[n]], tempRainDF[,modVar])
        tmpvec <- c(tmpvec, round(tau, 2))
      }
    }
    tmpvec <- c(regionnames[m], yVars[n], tmpvec)
    corr.rainfall[nrow(corr.rainfall)+1,] <- tmpvec
  }
}

write.csv(corr.rainfall, "Concatenated_Data/model_assessment/tau_diff_rain_functions.csv", row.names=F)
