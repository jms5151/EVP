rm(list=ls()) #remove previous variable assignments
source("Codes/climate_dependent_k_functions.R")

library(plyr)

# ecuadorClim <- read.csv("Ecuador/Machala_weather_station_data_2010-2011.csv", head=T)
# apply carrying capacity functions to temperature data ------------------------------
climateData <- read.csv("Kenya/Concatenated_Data/climate/gapfilled_climate_data.csv", head=T)
climateData$Date <- as.Date(climateData$Date, "%Y-%m-%d")

# temperature
climateData$Chulaimbo_temp_suitability <- unlist(lapply(climateData$GF_Chulaimbo_mean_temp, K_temp))
climateData$Kisumu_temp_suitability <- unlist(lapply(climateData$GF_Kisumu_mean_temp, K_temp))
climateData$Msambweni_temp_suitability <- unlist(lapply(climateData$GF_Msambweni_mean_temp, K_temp))
climateData$Ukunda_temp_suitability <- unlist(lapply(climateData$GF_Ukunda_mean_temp, K_temp))

# humidity
climateData$Chulaimbo_humidity_suitability <- unlist(lapply(climateData$GF_Chulaimbo_humidity, K_hum))
climateData$Kisumu_humidity_suitability <- unlist(lapply(climateData$GF_Kisumu_humidity, K_hum))
climateData$Msambweni_humidity_suitability <- unlist(lapply(climateData$GF_Msambweni_humidity, K_hum))
climateData$Ukunda_humidity_suitability <- unlist(lapply(climateData$GF_Ukunda_humidity, K_hum))

# load mosquito data ----------------------------------------------------------------
# source("Codes/concat_vector_data_from_redcap.R")
eggs_house <- read.csv("Kenya/Concatenated_Data/vector/house_vector_totals_eggs.csv", head=T, stringsAsFactors = F)
eggs_site <- read.csv("Kenya/Concatenated_Data/vector/site_vector_totals_eggs.csv", head=T)
larvae_house <- read.csv("Kenya/Concatenated_Data/vector/house_vector_totals_larvae.csv", head=T)
larvae_site <- read.csv("Kenya/Concatenated_Data/vector/site_vector_totals_larvae.csv", head=T)
adults_house <- read.csv("Kenya/Concatenated_Data/vector/house_vector_totals_adults.csv", head=T)
adults_site <- read.csv("Kenya/Concatenated_Data/vector/site_vector_totals_adults.csv", head=T)

# connect mosquito data to climate data ----------------------------------------------------
mosqDFs <- list(eggs_house, eggs_site, larvae_house, larvae_site, adults_house, adults_site)
trapNames <- c("eggs_house", "eggs_site", "larvae_house", "larvae_site", "adults_house", "adults_site")

for (j in 1:length(mosqDFs)){
  mosq.df <- mosqDFs[[j]]
  mosq.df[,"date_collected"] <- as.Date(mosq.df[,"date_collected"], "%Y-%m-%d")
  # mosq.df$study_site <- mapvalues(mosq.df$study_site, from=c(1:4), to=c("Ukunda", "Msambweni", "Chulaimbo", "Kisumu")) # replace multiple values at one time
  mosq.df[c("T_suitability", "Mean_temp", "H_suitability", "Mean_humidity", "R_suitability_briere", "R_suitability_hump", "R_suitability_linear", "Cumulative_rain")] <- NA
  for (k in 1:nrow(mosq.df)){
    collection.date <- mosq.df$date_collected[k]
    # temperature suitability
    TH.period.end <- collection.date - (2*7) # 2 weeks * 7 days/week
    TH.period.start <- TH.period.end - (4*7)-1 # 30 days
    TH_DF <- subset(climateData, Date >= TH.period.start & Date <= TH.period.end)
    tempName <- paste0(mosq.df$study_site[k], "_temp_suitability")
    mosq.df$T_suitability[k] <- sum(TH_DF[,tempName])/nrow(TH_DF)
    gfTempName <- paste0("GF_", mosq.df$study_site[k], "_mean_temp")
    mosq.df$Mean_temp[k] <- mean(TH_DF[,gfTempName])
    humName <- paste0(mosq.df$study_site[k], "_humidity_suitability")
    mosq.df$H_suitability[k] <- sum(TH_DF[,humName])/nrow(TH_DF)
    gfHumName <- paste0("GF_", mosq.df$study_site[k], "_humidity")
    mosq.df$Mean_humidity[k] <- mean(TH_DF[,gfHumName])
    # rain suitability
    rain.period.end <- collection.date - (4*7) 
    rain.period.start <- rain.period.end - (4*7)+1 # 30 days
    R_DF <- subset(climateData, Date >= rain.period.start & Date <= rain.period.end)
    rainName <- paste0("GF_", mosq.df$study_site[k], "_rain")
    cumRain <- sum(R_DF[,rainName])
    # rain <- c(rain, cumRain) # if running into NANs check that the cumulative rainfall is more than 500
    mosq.df$R_suitability_briere[k] <- K_rain_briere(cumRain)
    mosq.df$R_suitability_hump[k] <- K_rain_hump(cumRain)  
    # mosq.df$R_suitability_invHump[k] <- K_rain_invert_hump(cumRain)
    mosq.df$R_suitability_linear[k] <- K_rain_linear(cumRain)
    mosq.df$Cumulative_rain[k] <- cumRain
  }
  filePath <- paste0("Kenya/Concatenated_Data/vector/K_suitability_", trapNames[j], ".csv")
  write.csv(mosq.df, filePath, row.names = F)
  assign(paste0(trapNames[j], "_k_df"), mosq.df)
}

# add temporal lags (and quantify for autocorrelation) -----------------------
library(stringr)
library(Hmisc)

# site level -------------
sitedata <- list(adults_site_k_df, larvae_site_k_df, eggs_site_k_df)
sitedatanames <- c("adults_site_lags", "larvae_site_lags", "eggs_site_lags")

for (m in 1:length(sitedata)){
  datafill <- data.frame()
  dataSet <- substr(sitedatanames[m], 1,1)
  tempdf <- sitedata[[m]]
  sites <- unique(tempdf$study_site)
  for (n in 1:length(sites)){
    sitedf <- subset(tempdf, study_site == sites[n])
    if (dataSet == 'a'){
      sitedf$lag1mean <- Lag(sitedf$mean_aedes, 1)
      sitedf$lag1prop <- Lag(sitedf$prop.pos, 1)
    } else if (dataSet == 'l'){
      sitedf$lag1meanEI <- Lag(sitedf$mean_early_instars, 1)
      sitedf$lag1ppEI <- Lag(sitedf$prop_pos_early_instars, 1)
      sitedf$lag1meanLI <- Lag(sitedf$mean_late_instars, 1)
      sitedf$lag1ppLI <- Lag(sitedf$prop_pos_late_instars, 1)
      sitedf$lag1meanPU <- Lag(sitedf$mean_pupae, 1)
      sitedf$lag1ppPU <- Lag(sitedf$prop_pos_pupae, 1)
    } else {
      sitedf$lag1mean <- Lag(sitedf$mean_eggs, 1)
      sitedf$lag1prop <- Lag(sitedf$prop.pos, 1)
    }
    datafill <- rbind(datafill, sitedf)
  }
  filePath <- paste0("Kenya/Concatenated_Data/vector/K_suitability_", sitedatanames[m], ".csv")
  write.csv(datafill, filePath, row.names = F)
  assign(sitedatanames[m], datafill)
}

# Add NA for training data if using strict subset (e.g., no month prior in dataset) ------
adults_site_lags$lag1mean[adults_site_lags$mn.yr == "2017-09"] <- NA
adults_site_lags$lag1prop[adults_site_lags$mn.yr == "2017-09"] <- NA
write.csv(adults_site_lags, "Kenya/Concatenated_Data/vector/K_suitability_adults_site_lags.csv", row.names = F)

larvae_site_lags$lag1meanEI[larvae_site_lags$mn.yr == "2017-08"] <- NA
larvae_site_lags$lag1meanLI[larvae_site_lags$mn.yr == "2017-08"] <- NA
larvae_site_lags$lag1meanPU[larvae_site_lags$mn.yr == "2017-08"] <- NA
larvae_site_lags$lag1ppEI[larvae_site_lags$mn.yr == "2017-08"] <- NA
larvae_site_lags$lag1ppLI[larvae_site_lags$mn.yr == "2017-08"] <- NA
larvae_site_lags$lag1ppPU[larvae_site_lags$mn.yr == "2017-08"] <- NA
write.csv(larvae_site_lags, "Kenya/Concatenated_Data/vector/K_suitability_larvae_site_lags.csv", row.names = F)

eggs_site_lags$lag1mean[eggs_site_lags$mn.yr == "2016-05"] <- NA
eggs_site_lags$lag1prop[eggs_site_lags$mn.yr == "2016-05"] <- NA
write.csv(eggs_site_lags, "Kenya/Concatenated_Data/vector/K_suitability_eggs_site_lags.csv", row.names = F)

# correlations ---------------------------------------------------------------------------
lagdfs <- list(adults_site_lags, adults_site_lags, eggs_site_lags, eggs_site_lags, larvae_site_lags, larvae_site_lags, larvae_site_lags, larvae_site_lags, larvae_site_lags, larvae_site_lags) 
xvars <- c("mean_aedes", "prop.pos", "mean_eggs", "prop.pos", "mean_early_instars", "prop_pos_early_instars", "mean_late_instars", "prop_pos_early_instars", "mean_pupae", "prop_pos_pupae")
yvars <- c("lag1mean", "lag1prop", "lag1mean", "lag1prop", "lag1meanEI", "lag1ppEI", "lag1meanLI", "lag1ppLI", "lag1meanPU", "lag1ppPU")

corrtab<-data.frame(cbind(xvars, yvars))
colnames(corrtab) <- c("Var", "LaggedVar")
corrtab$Correlation <- NA

for (p in 1:length(lagdfs)){
  corrDF <- lagdfs[[p]]
  x <- cor.test(corrDF[,xvars[p]], corrDF[,yvars[p]])
  corrtab$Correlation[p] <- unname(unlist(x)["estimate.cor"])
}

write.csv(corrtab, "Kenya/Concatenated_Data/vector/model_output/site_level_autocorrelation.csv", row.names = F)

# house level -----------------------
housedata <- list(adults_house_k_df, larvae_house_k_df, eggs_house_k_df)
housedatanames <- c("adults_house_lags", "larvae_house_lags", "eggs_house_lags")

for (o in 1:length(housedata)){
  datafill <- data.frame()
  dataSet <- substr(housedatanames[o], 1,1)
  tempdf <- housedata[[o]]
  sites <- unique(tempdf$study_site)
  for (p in 1:length(sites)){
    sitedf <- subset(tempdf, study_site == sites[p])
    houses <- unique(sitedf$vector_house_id)
    for (q in 1:length(houses)){
      housedf <- subset(sitedf, vector_house_id == houses[q])
      if (dataSet == 'a'){
        housedf$lag1total <- Lag(housedf$aedes_total, 1)
      } else if (dataSet == 'l'){
        housedf$lag1EI <- Lag(housedf$early_instar_total, 1)
        housedf$lag1LI <- Lag(housedf$late_instar_total, 1)
        housedf$lag1PU <- Lag(housedf$pupae_total, 1)
      } else {
        housedf$lag1total <- Lag(housedf$egg_total, 1)
      }
      datafill <- rbind(datafill, housedf)
    }
  }
  filePath <- paste0("Kenya/Concatenated_Data/vector/K_suitability_", housedatanames[o], ".csv")
  write.csv(datafill, filePath, row.names = F)
  assign(housedatanames[o], datafill)
}

# Add NA for training data if using strict subset (e.g., no month prior in dataset) ------
adults_house_lags$lag1total[adults_house_lags$mn.yr == "2017-09"] <- NA
write.csv(adults_house_lags, "Kenya/Concatenated_Data/vector/K_suitability_adults_house_lags.csv", row.names = F)

larvae_house_lags$lag1EI[larvae_house_lags$mn.yr == "2017-08"] <- NA
larvae_house_lags$lag1LI[larvae_house_lags$mn.yr == "2017-08"] <- NA
larvae_house_lags$lag1PU[larvae_house_lags$mn.yr == "2017-08"] <- NA
write.csv(larvae_house_lags, "Kenya/Concatenated_Data/vector/K_suitability_larvae_house_lags.csv", row.names = F)

eggs_house_lags$lag1total[eggs_house_lags$mn.yr == "2016-05"] <- NA
write.csv(eggs_house_lags, "Kenya/Concatenated_Data/vector/K_suitability_eggs_house_lags.csv", row.names = F)

# correlations ---------------------------------------------------------------------------
lagdfs <- list(adults_house_lags, eggs_house_lags, larvae_house_lags, larvae_house_lags, larvae_house_lags) 
xvars <- c("aedes_total", "egg_total", "early_instar_total", "late_instar_total", "pupae_total")
yvars <- c("lag1total", "lag1total", "lag1EI", "lag1LI", "lag1PU")

corrtab2<-data.frame(cbind(xvars, yvars))
colnames(corrtab2) <- c("Var", "LaggedVar")
corrtab2$Correlation <- NA

for (p in 1:length(lagdfs)){
  corrDF <- lagdfs[[p]]
  x <- cor.test(corrDF[,xvars[p]], corrDF[,yvars[p]])
  corrtab2$Correlation[p] <- unname(unlist(x)["estimate.cor"])
}

write.csv(corrtab2, "Kenya/Concatenated_Data/vector/model_output/house_level_autocorrelation.csv", row.names = F)

# plot ----------------------------------------------------------------------
# mosq.df <- prokopack_k_df
# mosquitoes <- "egg_total"
# mosquitoes <- "aedes_total"
# 
# plot(mosq.df$T_suitability, mosq.df[,mosquitoes])
# plot(mosq.df$H_suitability, mosq.df[,mosquitoes])
# plot(mosq.df$R_suitability_briere, mosq.df[,mosquitoes])
# plot(mosq.df$R_suitability_hump, mosq.df[,mosquitoes])
# plot(mosq.df$R_suitability_invHump, mosq.df[,mosquitoes])
# plot(mosq.df$R_suitability_linear, mosq.df[,mosquitoes])