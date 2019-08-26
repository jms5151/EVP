# concat z-scores with climate data for CART models --------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(plyr)

# load data
load("Concatenated_Data/model_v_data.RData")

# functions
zscore <- function(x){(x - mean(x,na.rm=T)) / sd(x,na.rm=T)}

# characterize correspondance based on z-scores for model and data ----------------------
sites <- unique(merged_data$Site)
zscoreDF <- data.frame()

for (siteX in sites){
  zscoreSub <- subset(merged_data, Site == siteX)
  if (siteX == "Zaruma"){
    zscoreSub[,c("zscore_aedes_total", "zscore_denv_positive", "zscore_Mtot", "zscore_I")] <- zscoreSub[,c("aedes_total", "denv_positive", "Mtot", "I")]
  } else {
    zscoreSub[,c("zscore_aedes_total", "zscore_denv_positive", "zscore_Mtot", "zscore_I")] <- lapply(zscoreSub[,c("aedes_total", "denv_positive", "Mtot", "I")], zscore)
  }   
  zscoreDF <- rbind(zscoreDF, zscoreSub)
}

# categorize by correspondance
zscoreDF$Adult_correspondence_magnitude <- ifelse(zscoreDF$zscore_aedes_total-zscoreDF$zscore_Mtot < -1, "Overprediction", NA)
zscoreDF$Adult_correspondence_magnitude <- ifelse(zscoreDF$zscore_aedes_total-zscoreDF$zscore_Mtot > 1, "Underprediction", zscoreDF$Adult_correspondence_magnitude)
zscoreDF$Adult_correspondence_magnitude <- ifelse(zscoreDF$zscore_aedes_total-zscoreDF$zscore_Mtot >= -1 & zscoreDF$zscore_aedes_total-zscoreDF$zscore_Mtot <= 1, "Correspondence", zscoreDF$Adult_correspondence_magnitude)

zscoreDF$Dengue_correspondence_magnitude <- ifelse(zscoreDF$zscore_denv_positive-zscoreDF$zscore_I < -1, "Overprediction", NA)
zscoreDF$Dengue_correspondence_magnitude <- ifelse(zscoreDF$zscore_denv_positive-zscoreDF$zscore_I > 1, "Underprediction", zscoreDF$Dengue_correspondence_magnitude)
zscoreDF$Dengue_correspondence_magnitude <- ifelse(zscoreDF$zscore_denv_positive-zscoreDF$zscore_I >= -1 & zscoreDF$zscore_denv_positive-zscoreDF$zscore_I <= 1, "Correspondence", zscoreDF$Dengue_correspondence_magnitude)

# subset zscore data
zscoreSub <- zscoreDF[,c("Site", "Date", "Adult_correspondence_magnitude", "Dengue_correspondence_magnitude")]
zscoreSub <- subset(zscoreSub, !is.na(Adult_correspondence_magnitude) | !is.na(Dengue_correspondence_magnitude))

# calculate climate conditions preceeding each observation
load("Concatenated_Data/Climate_data/merged_climate_data.RData")

vars <- list(b = c("min", "mean", "max", "var"), c = c("T", "H", "R"))
climVars <- data.frame(do.call(expand.grid, vars))
clim_vars <- c(paste(climVars$c, climVars$b, sep="_"))#, "R_cum", "R_sum_days_1", "R_sum_days_3", "R_sum_days_5", "R_sum_days_10")
zscoreSub <- cbind(zscoreSub, setNames(lapply(clim_vars, function(x) x=NA), clim_vars) )
# daily_clim_vars <- c(rep("mean_temp", 4), rep("humidity", 4), rep("rain", 4))
clim <- c("Temperature", "SVPD", "Two_week_rainfall")
clim2 <- c("T", "H", "R")  

for (i in 1:nrow(zscoreSub)){
  clim.dat <- subset(climateData, Site == zscoreSub$Site[i])
  climRowIndexLast <- which(zscoreSub$Date[i] == clim.dat$Date) 
  climRowIndexFirst <- climRowIndexLast - 29
  for (j in 1:length(clim)){
    zscoreSub[i, paste0(clim2[j], "_min")] <- min(clim.dat[climRowIndexFirst:climRowIndexLast, clim[j]])
    zscoreSub[i, paste0(clim2[j], "_mean")] <- mean(clim.dat[climRowIndexFirst:climRowIndexLast, clim[j]])
    zscoreSub[i, paste0(clim2[j], "_max")] <- max(clim.dat[climRowIndexFirst:climRowIndexLast, clim[j]])
    zscoreSub[i, paste0(clim2[j], "_var")] <- var(clim.dat[climRowIndexFirst:climRowIndexLast, clim[j]])
  }
}

# save data
save(zscoreSub,  file="Concatenated_Data/zscores_with_climate_data.RData")
