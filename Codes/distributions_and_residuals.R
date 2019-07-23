## add in to concat_zscore_climate_data between lines 13 and 14
# zscoreSub <- zscoreDF
# zscoreSub$aedesDiff <- zscoreSub$aedes_total_weekly_zscore-zscoreSub$Mtot_THR_zscore
# zscoreSub$dengueDiff <- zscoreSub$denv_positive_weekly_any_zscore-zscoreSub$I_THR_zscore
climateData <- read.csv("Concatenated_Data/climate_data/merged_climate_data.csv", head=T, stringsAsFactors = F)

# tempmodel <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_temperature.csv", head=T, stringsAsFactors = F)
# tempmodel$Mtot <- tempmodel$M1 + tempmodel$M2 + tempmodel$M3

data <- merge(cases, vectors, by=c("Site", "Date"), all=T)
data <- merge(data, models2, by=c("Site", "Date"), all=T)

data$CumRain <- NA
data$temp <- NA

for (i in 7:nrow(data)){
  climRowIndex <- which(data$Date[i] == climateData$Date) - 6
  data$CumRain[i] <- climateData[climRowIndex, paste0("GF_", data$Site[i], "_cumRain")]
  data$temp[i] <- climateData[climRowIndex, paste0("GF_", data$Site[i], "_mean_temp")]
}

# data$diff <- ifelse(!is.na(data$aedes_total_monthly), data$Mtot - data$aedes_total_monthly, NA)
data$site2 <- data$Site
data$site2[data$site2 == "Chulaimbo"] <- 1
data$site2[data$site2 == "Kisumu"] <- 2
data$site2[data$site2 == "Msambweni"] <- 3
data$site2[data$site2 == "Ukunda"] <- 4
data$site2[data$site2 == "Huaquillas"] <- 5
data$site2[data$site2 == "Machala"] <- 6
data$site2[data$site2 == "Portovelo"] <- 7
data$site2[data$site2 == "Zaruma"] <- 8
data$site2 <- as.numeric(data$site2)

data$diff <- ifelse(!is.na(data$aedes_total_weekly), data$Mtot_cumRain_K_thr_linear_increasing_85 - data$aedes_total_weekly, NA)

plot(data$CumRain, data$diff, pch=16, col=data$site2, ylab=c("Model - Observations for Aedes aegypti"), xlab=c("Cumulative rainfall"))
abline(0,1)

par(mfrow=c(2,3))
hist(simobs$Mtot_THR, main="", col='grey', xlab='Modeled mosquitoes')
hist(simobs$aedes_total_weekly, main="", col='grey', xlab='Adults')
hist(simobs$pupae_total_weekly, main="", col='grey', xlab='Pupae')
hist(simobs$late_instar_total_weekly, main="", col='grey', xlab='Late instars')
hist(simobs$early_instar_total_weekly, main="", col='grey', xlab='Early instars')
hist(simobs$egg_total_weekly, main="", col='grey', xlab='Eggs')


hist(simobs$I_THR, main="", col='grey', xlab='Modeled cases')
hist(simobs$denv_positive_weekly_any, main="", col='grey', xlab='Dengue (all)')
hist(simobs$denv_positive_0311_weekly, main="", col='grey', xlab='Dengue 2003-2011')
hist(simobs$denv_positive_1418_weekly, main="", col='grey', xlab='Dengue 2014-2018')
hist(simobs$chikv_positive_weekly, main="", col='grey', xlab='Chikungunya')

hist(simobs$I_THR_zscore)

plot(zscoreSub$H_mean, zscoreSub$dengueDiff, pch=16)
abline(0,0)
# abline(v=57, lty=2)

plot(zscoreSub$H_mean, zscoreSub$aedesDiff, pch=16)
abline(0,0)

plot(zscoreSub$chikv_positive_weekly, zscoreSub$I_THR, pch=16)
