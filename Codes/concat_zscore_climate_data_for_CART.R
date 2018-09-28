# concat z-scores with climate data for CART models --------------------------------------
rm(list=ls()) #remove previous variable assignments

# load data ------------------------------------------------------------------------------
# source("C:/Users/Jamie/Box Sync/DENV/Codes/calculate_zscores.R")
zscoreDF <- read.csv("Kenya/Concatenated_Data/zscores_model_v_data_1SD.csv", head=T)
climateData <- read.csv("Kenya/Concatenated_Data/climate/gapfilled_climate_data.csv", head=T)

# format dates
zscoreDF$Date <- as.Date(zscoreDF$Date, "%Y-%m-%d")
climateData$Date <- as.Date(climateData$Date, "%Y-%m-%d")

# calculate climate conditions for each date ---------------------------------------------
xx = c("mean_T_mosq", "var_T_mosq", "mean_H_mosq", "var_H_mosq", "cum_R_mosq", "var_R_mosq","mean_T_cases", "var_T_cases", "mean_H_cases", "var_H_cases", "cum_R_cases", "var_R_cases")
zscoreDF <- cbind(zscoreDF, setNames(lapply(xx, function(x) x=NA), xx) )

meanTemp <- c("mean_T_mosq", "mean_T_cases")
varTemp <- c("var_T_mosq", "var_T_cases")
meanHum <- c("mean_H_mosq", "mean_H_cases")
varHum <- c("var_H_mosq", "var_H_cases")
cumRain <- c("cum_R_mosq", "cum_R_cases")
varRain <- c("var_R_mosq", "var_R_cases")

startRow <- min(which(zscoreDF$Date == "2014-01-01"))

for (i in startRow:nrow(zscoreDF)){
  site <- zscoreDF$study_site[i]
  collection.date <- zscoreDF$Date[i]
  for (j in 1:length(meanTemp)){
    if (j == 1){
      period.end <- collection.date - 7
      period.start <- period.end - (4*7)-1 # 30 days
    } else {
      period.end <- collection.date - 14
      period.start <- period.end - (4*7)-1 # 30 days
    }
    THR <- subset(climateData, Date >= period.start & Date <= period.end)
    tempName <- paste0("GF_", site, "_mean_temp")
    zscoreDF[i,meanTemp[j]] <- mean(THR[,tempName], na.rm=T)
    zscoreDF[i, varTemp[j]] <- var(THR[,tempName], na.rm=T)
    humName <- paste0("GF_", site, "_humidity")
    zscoreDF[i, meanHum] <- mean(THR[,humName], na.rm=T)
    zscoreDF[i, varHum] <- var(THR[,humName], na.rm=T)
    rainName <- paste0("GF_", site, "_rain")
    zscoreDF[i, cumRain] <- sum(THR[,rainName], na.rm=T)
    zscoreDF[i, varRain] <- var(THR[,rainName], na.rm=T)
  }
}

# write to csv -------------------------------------------------------------------------
write.csv(zscoreDF,  "Kenya/Concatenated_Data/zscores_with_climate_data.csv", row.names = F)
