# concat z-scores with climate data for CART models --------------------------------------
rm(list=ls()) #remove previous variable assignments

# load data
zscoreDF <- read.csv("Concatenated_Data/model_v_data_zscores.csv", head=T)
climateData <- read.csv("Concatenated_Data/Climate_data/merged_climate_data.csv", head=T)

# format dates
zscoreDF$Date <- as.Date(zscoreDF$Date, "%Y-%m-%d")
climateData$Date <- as.Date(climateData$Date, "%Y-%m-%d")

# subset zscore data
zscoreSub <- zscoreDF[,c("Site", "Date", "Adult_correspondence_magnitude", "Dengue_correspondence_magnitude")]
zscoreSub <- subset(zscoreSub, !is.na(Adult_correspondence_magnitude) | !is.na(Dengue_correspondence_magnitude))
# zscoreSub <- subset(zscoreDF, !is.na(aedes_total_weekly)|!is.na(denv_positive_weekly_any)|!is.na(denv_positive_monthly))

# calculate climate conditions preceeding each observation
vars <- list(b = c("min", "mean", "max", "var"), c = c("T", "H", "R"))
climVars <- data.frame(do.call(expand.grid, vars))
clim_vars <- c(paste(climVars$c, climVars$b, sep="_"), "R_cum", "R_sum_days")
zscoreSub <- cbind(zscoreSub, setNames(lapply(clim_vars, function(x) x=NA), clim_vars) )
# daily_clim_vars <- c(rep("mean_temp", 4), rep("humidity", 4), rep("rain", 4))
clim <- c("mean_temp", "humidity", "rain")
clim2 <- c("T", "H", "R")  

for (i in 1:nrow(zscoreSub)){
  climRowIndexLast <- which(zscoreSub$Date[i] == climateData$Date) 
  climRowIndexFirst <- climRowIndexLast - 29
  for (j in 1:length(clim)){
    climateDataColName <- paste("GF", zscoreSub$Site[i], clim[j], sep="_")
    zscoreSub[i, paste0(clim2[j], "_min")] <- min(climateData[climRowIndexFirst:climRowIndexLast, climateDataColName])
    zscoreSub[i, paste0(clim2[j], "_mean")] <- mean(climateData[climRowIndexFirst:climRowIndexLast, climateDataColName])
    zscoreSub[i, paste0(clim2[j], "_max")] <- max(climateData[climRowIndexFirst:climRowIndexLast, climateDataColName])
    zscoreSub[i, paste0(clim2[j], "_var")] <- var(climateData[climRowIndexFirst:climRowIndexLast, climateDataColName])
    if (clim2[j] == "R"){
      zscoreSub[i, paste0(clim2[j], "_cum")] <- sum(climateData[climRowIndexFirst:climRowIndexLast, climateDataColName])
      zscoreSub[i, paste0(clim2[j], "_sum_days")] <- sum(climateData[climRowIndexFirst:climRowIndexLast, climateDataColName]>1) # number of precipidating days greater than 1 mm/day
    }
  }
}

# save data
write.csv(zscoreSub,  "Concatenated_Data/zscores_with_climate_data.csv", row.names = F)

# zscoreSub$Country <- ifelse(zscoreSub$Site=="Chulaimbo"|zscoreSub$Site=="Kisumu"|zscoreSub$Site=="Msambweni"|zscoreSub$Site=="Ukunda", "Kenya", "Ecuador")
# test<-subset(zscoreSub, Country == "Kenya")
# plot(test$R_cum, test$denv_positive_monthly, pch=16, col=test$Site)
# 
# test2<-merge(test[,c("Date", "aedes_total_monthly", "pupae_total_monthly", "late_instar_total_monthly", "early_instar_total_monthly", "egg_total_monthly")], climateData, by="Date")
# plot(test2$GF_Portovelo_cumRain, test2$aedes_total_monthly)
# plot(test2$GF_Huaquillas_cumRain, test2$pupae_total_monthly)
# plot(test2$GF_Huaquillas_cumRain, test2$late_instar_total_monthly)
# plot(test2$GF_Huaquillas_cumRain, test2$early_instar_total_monthly)
# plot(test2$GF_Huaquillas_cumRain, test2$egg_total_monthly)
# meanTemp <- c("mean_T_mosq", "mean_T_cases")
# varTemp <- c("var_T_mosq", "var_T_cases")
# meanHum <- c("mean_H_mosq", "mean_H_cases")
# varHum <- c("var_H_mosq", "var_H_cases")
# cumRain <- c("cum_R_mosq", "cum_R_cases")
# varRain <- c("var_R_mosq", "var_R_cases")

# startRow <- min(which(zscoreDF$Date == "2014-01-01"))
# for (i in startRow:nrow(zscoreDF)){
#   site <- zscoreDF$study_site[i]
#   collection.date <- zscoreDF$Date[i]
#   for (j in 1:length(meanTemp)){
#     if (j == 1){
#       period.end <- collection.date - 7
#       period.start <- period.end - (4*7)-1 # 30 days
#     } else {
#       period.end <- collection.date - 14
#       period.start <- period.end - (4*7)-1 # 30 days
#     }
#     THR <- subset(climateData, Date >= period.start & Date <= period.end)
#     tempName <- paste0("GF_", site, "_mean_temp")
#     zscoreDF[i,meanTemp[j]] <- mean(THR[,tempName], na.rm=T)
#     zscoreDF[i, varTemp[j]] <- var(THR[,tempName], na.rm=T)
#     humName <- paste0("GF_", site, "_humidity")
#     zscoreDF[i, meanHum] <- mean(THR[,humName], na.rm=T)
#     zscoreDF[i, varHum] <- var(THR[,humName], na.rm=T)
#     rainName <- paste0("GF_", site, "_rain")
#     zscoreDF[i, cumRain] <- sum(THR[,rainName], na.rm=T)
#     zscoreDF[i, varRain] <- var(THR[,rainName], na.rm=T)
#   }
# }

# write to csv -------------------------------------------------------------------------
# write.csv(zscoreDF,  "Concatenated_Data/zscores_with_climate_data.csv", row.names = F)
