# concatenate climate data from redcap and gapfill missing logger data --------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(tidyverse)
library(plyr)

# load and format data redcap data ---------------------------------------------------------------------
# import redcap data as 'redcap_clim_vec'
source("Codes/REDCap_import_climate_data.R")

# create site and date variables
redcap_climate$site <- gsub("_arm_1", "", redcap_climate$redcap_event_name)
redcap_climate$Date <- redcap_climate$date_collected

# subset data to weather variables of interest 
climate_subset <- redcap_climate[,c("Date", "site", "temp_mean_hobo", "rainfall_hobo", "daily_rainfall", "rh_mean_hobo")]

# load and format NOAA GSOD (Global Summary of the Day) from ------------------------------------------
# https://www7.ncdc.noaa.gov/CDO/cdoselect.cmd?datasetabbv=GSOD&countryabbv&georegionabbv
kisumu_gsod <- read.delim("Kenya/Climate/GSOD_Kisumu.txt", header = TRUE, sep = ",")
mombasa_gsod <- read.delim("Kenya/Climate/GSOD_Mombasa.txt", header = TRUE, sep = ",")
# kisumu_gsod <- read.delim("C:/Users/Jeremy/Desktop/GSOD_Kisumu.txt", header = TRUE, sep = ",")
# mombasa_gsod <- read.delim("C:/Users/Jeremy/Desktop/GSOD_Mombasa.txt", header = TRUE, sep = ",")

# format temperature and site
gsod.files <- list(kisumu_gsod, mombasa_gsod)
gsod.files <-lapply(gsod.files, function(x) cbind(x, mean_temp_gsod = (x$TEMP-32)*(5/9)))
gsod.files <-lapply(gsod.files, function(x) cbind(x, Date = as.Date(paste(substr(x$YEARMODA, 1, 4), substr(x$YEARMODA, 5, 6), substr(x$YEARMODA, 7, 8), sep="-"), "%Y-%m-%d")))

# split, subset data, and merge data
kisumu_gsod <- gsod.files[[1]][,c("Date", "mean_temp_gsod")]
mombasa_gsod <- gsod.files[[2]][,c("Date", "mean_temp_gsod")]
wide_data <- merge(kisumu_gsod, mombasa_gsod, by="Date", all=T)
colnames(wide_data) <- c("Date", "kisumu_mean_temp_gsod", "mombasa_mean_temp_gsod")

# reshape from long to wide
sites <- unique(climate_subset$site)

for (i in 1:length(sites)){
  x <- subset(climate_subset, site == sites[i])
  x <- x[, !names(x) %in% c("site")]
  colnames(x)[2:5] <- paste(sites[i], colnames(x)[2:5], sep='_')
  wide_data <- merge(wide_data, x, by="Date", all=T)
}

# make sure every date is included ------------------------------------------------------------------
minDate <- as.Date('2013-06-01', '%Y-%m-%d')
maxDate <- as.Date('2019-02-11', '%Y-%m-%d')
dates <- as.data.frame(seq.Date(minDate, maxDate, by=1))
colnames(dates)[1] <- "Date"
wide_data <- merge(wide_data, dates, by="Date", all=T)
wide_data <- subset(wide_data, Date >= minDate & Date <= maxDate)

# fill in missing temperature data ------------------------------------------------------------------
# Chulaimbo
fill_ch_w_hosp <- lm(chulaimbo_village_temp_mean_hobo ~ chulaimbo_hospital_temp_mean_hobo, data = wide_data)
fill_ch_w_gsod <- lm(chulaimbo_village_temp_mean_hobo ~ kisumu_mean_temp_gsod, data = wide_data)
wide_data$chulaimbo_Temperature <- ifelse(!is.na(wide_data$chulaimbo_village_temp_mean_hobo), wide_data$chulaimbo_village_temp_mean_hobo, round(coef(fill_ch_w_hosp)[[1]] + coef(fill_ch_w_hosp)[[2]] * wide_data$chulaimbo_hospital_temp_mean_hobo, 1))
wide_data$chulaimbo_Temperature <- ifelse(!is.na(wide_data$chulaimbo_Temperature), wide_data$chulaimbo_Temperature, round(coef(fill_ch_w_gsod)[[1]] + coef(fill_ch_w_gsod)[[2]] * wide_data$kisumu_mean_temp_gsod, 1))

# Kisumu
wide_data$kisumu_estate_temp_mean_hobo <- ifelse(wide_data$kisumu_estate_temp_mean_hobo < 18, NA, wide_data$kisumu_estate_temp_mean_hobo) # remove suspect temperature values 
fill_ki_w_obama <- lm(kisumu_estate_temp_mean_hobo ~ obama_temp_mean_hobo, data = wide_data)
fill_ki_w_gsod <- lm(kisumu_estate_temp_mean_hobo ~ kisumu_mean_temp_gsod, data = wide_data)
wide_data$kisumu_Temperature <- ifelse(!is.na(wide_data$kisumu_estate_temp_mean_hobo), wide_data$kisumu_estate_temp_mean_hobo, round(coef(fill_ki_w_obama)[[1]] + coef(fill_ki_w_obama)[[2]] * wide_data$obama_temp_mean_hobo, 1))
wide_data$kisumu_Temperature <- ifelse(!is.na(wide_data$kisumu_Temperature), wide_data$kisumu_Temperature, round(coef(fill_ki_w_gsod)[[1]] + coef(fill_ki_w_gsod)[[2]] * wide_data$kisumu_mean_temp_gsod, 1))

# Msambweni
fill_ms_w_uk <- lm(msambweni_temp_mean_hobo ~ ukunda_temp_mean_hobo, data = wide_data)
fill_ms_w_gsod <- lm(msambweni_temp_mean_hobo ~ mombasa_mean_temp_gsod, data = wide_data)
wide_data$msambweni_Temperature <- ifelse(!is.na(wide_data$msambweni_temp_mean_hobo), wide_data$msambweni_temp_mean_hobo, round(coef(fill_ms_w_uk)[[1]] + coef(fill_ms_w_uk)[[2]] * wide_data$ukunda_temp_mean_hobo, 1))
wide_data$msambweni_Temperature <- ifelse(!is.na(wide_data$msambweni_Temperature), wide_data$msambweni_Temperature, round(coef(fill_ms_w_gsod)[[1]] + coef(fill_ms_w_gsod)[[2]] * wide_data$mombasa_mean_temp_gsod, 1))

# Ukunda
wide_data$ukunda_temp_mean_hobo <- ifelse(wide_data$ukunda_temp_mean_hobo >= 34, NA, wide_data$ukunda_temp_mean_hobo) # remove suspect temperature values 
fill_uk_w_gsod <- lm(ukunda_temp_mean_hobo ~ mombasa_mean_temp_gsod, data = wide_data)
wide_data$ukunda_Temperature <- ifelse(!is.na(wide_data$ukunda_temp_mean_hobo), wide_data$ukunda_temp_mean_hobo, round(coef(fill_uk_w_gsod)[[1]] + coef(fill_uk_w_gsod)[[2]] * wide_data$mombasa_mean_temp_gsod, 1))

# fill in the few missing temperature days with the mean of the 2 days before and after date with missing data
sites <- c("chulaimbo", "kisumu", "msambweni", "ukunda")

for (j in 1:length(sites)){
  for (k in 3:nrow(wide_data)){
    if (is.na(wide_data[k,paste0(sites[j], "_Temperature")])){
      wide_data[k,paste0(sites[j], "_Temperature")] <- mean(wide_data[(k-2):(k+2),paste0(sites[j], "_Temperature")], na.rm=T)
    }  
  }
}  

# fill in missing rainfall data -------------------------------------------------------------------
# calculate 30 days aggregated rainfall values
wide_data$chulaimbo_rainfall_hobo <- wide_data$chulaimbo_village_rainfall_hobo
wide_data$chulaimbo_daily_rainfall <- wide_data$chulaimbo_hospital_daily_rainfall 
sites2 <- c("chulaimbo", "obama", "msambweni", "ukunda")

for (j in 1:length(sites2)){
  wide_data[paste0("Two_week_rainfall_", sites2[j])] <- NA
  wide_data[paste0("Two_week_rainfall_", sites2[j], "_noaa")] <- NA
  for (k in 14:nrow(wide_data)){
    wide_data[k,paste0("Two_week_rainfall_", sites2[j])] <- sum(wide_data[(k-13):k, paste0(sites2[j], "_rainfall_hobo")])
    wide_data[k,paste0("Two_week_rainfall_", sites2[j], "_noaa")] <- sum(wide_data[(k-13):k, paste0(sites2[j], "_daily_rainfall")])
  }  
}  

# Chulaimbo
fill_ch_w_noaa <- lm(Two_week_rainfall_chulaimbo ~ Two_week_rainfall_chulaimbo_noaa, data = wide_data)
wide_data$chulaimbo_Two_week_rainfall <- ifelse(!is.na(wide_data$Two_week_rainfall_chulaimbo), wide_data$Two_week_rainfall_chulaimbo, round(coef(fill_ch_w_noaa)[[1]] + coef(fill_ch_w_noaa)[[2]] * wide_data$Two_week_rainfall_chulaimbo_noaa, 1))
# replace values below zero with zero (due to negative coefficient value in regression)
wide_data$chulaimbo_Two_week_rainfall <- ifelse(wide_data$chulaimbo_Two_week_rainfall < 0, 0, wide_data$chulaimbo_Two_week_rainfall)

# Kisumu
fill_ki_w_noaa <- lm(Two_week_rainfall_obama ~ Two_week_rainfall_obama_noaa, data = wide_data)
wide_data$kisumu_Two_week_rainfall <- ifelse(!is.na(wide_data$Two_week_rainfall_obama), wide_data$Two_week_rainfall_obama, round(coef(fill_ki_w_noaa)[[1]] + coef(fill_ki_w_noaa)[[2]] * wide_data$Two_week_rainfall_obama_noaa, 1))
# replace values below zero with zero (due to negative coefficient value in regression)
wide_data$kisumu_Two_week_rainfall <- ifelse(wide_data$kisumu_Two_week_rainfall < 0, 0, wide_data$kisumu_Two_week_rainfall)

# Msmabweni
fill_ms_w_noaa <- lm(Two_week_rainfall_msambweni ~ Two_week_rainfall_msambweni_noaa, data = wide_data)
wide_data$msambweni_Two_week_rainfall <- ifelse(!is.na(wide_data$Two_week_rainfall_msambweni), wide_data$Two_week_rainfall_msambweni, round(coef(fill_ms_w_noaa)[[1]] + coef(fill_ms_w_noaa)[[2]] * wide_data$Two_week_rainfall_msambweni_noaa, 1))

# Ukunda
fill_uk_w_noaa <- lm(Two_week_rainfall_ukunda ~ Two_week_rainfall_ukunda_noaa, data = wide_data)
wide_data$ukunda_Two_week_rainfall <- ifelse(!is.na(wide_data$Two_week_rainfall_ukunda), wide_data$Two_week_rainfall_ukunda, round(coef(fill_uk_w_noaa)[[1]] + coef(fill_uk_w_noaa)[[2]] * wide_data$Two_week_rainfall_ukunda_noaa, 1))

# fill in missing humidity data -------------------------------------------------------------------
wide_data$Month_Day <- format(wide_data$Date, "%m-%d")
humidityMeans <- ddply(wide_data, .(Month_Day), summarize
                       , chulaimbo_rh_ltm = round(mean(chulaimbo_hospital_rh_mean_hobo, na.rm=T), mean(chulaimbo_village_rh_mean_hobo, na.rm=T))
                       , kisumu_rh_ltm = round(mean(obama_rh_mean_hobo, na.rm=T), mean(kisumu_estate_rh_mean_hobo, na.rm=T))
                       , msambweni_rh_ltm = round(mean(msambweni_rh_mean_hobo, na.rm=T))
                       , ukunda_rh_ltm = round(mean(ukunda_rh_mean_hobo, na.rm=T)))

wide_data <- merge(wide_data, humidityMeans, by="Month_Day", all=T)

wide_data$chulaimbo_Humidity <- ifelse(!is.na(wide_data$chulaimbo_village_rh_mean_hobo), wide_data$chulaimbo_village_rh_mean_hobo, wide_data$chulaimbo_rh_ltm)
wide_data$kisumu_Humidity <- ifelse(!is.na(wide_data$obama_rh_mean_hobo), wide_data$obama_rh_mean_hobo, wide_data$kisumu_rh_ltm)
wide_data$msambweni_Humidity <- ifelse(!is.na(wide_data$msambweni_rh_mean_hobo), wide_data$msambweni_rh_mean_hobo, wide_data$msambweni_rh_ltm)
wide_data$ukunda_Humidity <- ifelse(!is.na(wide_data$ukunda_rh_mean_hobo), wide_data$ukunda_rh_mean_hobo, wide_data$ukunda_rh_ltm)

# save pre-gapfilled data
# x<-wide_data[,c("Date", "chulaimbo_village_temp_mean_hobo", "kisumu_estate_temp_mean_hobo", "kisumu_mean_temp_gsod", "msambweni_temp_mean_hobo", "ukunda_temp_mean_hobo", "mombasa_mean_temp_gsod", "Two_week_rainfall_chulaimbo", "Two_week_rainfall_chulaimbo_noaa", "Two_week_rainfall_obama", "Two_week_rainfall_obama_noaa", "Two_week_rainfall_msambweni", "Two_week_rainfall_msambweni_noaa", "Two_week_rainfall_ukunda", "Two_week_rainfall_ukunda_noaa", "chulaimbo_village_rh_mean_hobo", "obama_rh_mean_hobo", "msambweni_rh_mean_hobo", "ukunda_rh_mean_hobo")]
# write.csv(x, "Concatenated_Data/climate_data/pregapfilled_data_Kenya.csv", row.names = F)

# merge data into long format and save -----------------------------------------------------------
wide_data <- wide_data[order(wide_data$Date),]

for (j in 1:length(sites)){
  weatherdf <- wide_data[, c("Date", paste0(sites[j], "_Temperature"), paste0(sites[j], "_Two_week_rainfall"), paste0(sites[j], "_Humidity"))]
  colnames(weatherdf) <- gsub(paste0(sites[j], "_"), "", colnames(weatherdf))
  weatherdf$Site <- paste0(toupper(substr(sites[j],1,1)), substr(sites[j],2,nchar(sites[j])))
  weatherdf <- weatherdf[14:nrow(weatherdf),]
  assign(sites[j], weatherdf)
}  

weatherdf <- do.call(rbind, list(chulaimbo, kisumu, msambweni, ukunda))
weatherdf[,c(2:4)] <- lapply(weatherdf[,c(2:4)], function(x) round(x,2))
write.csv(weatherdf, "Concatenated_Data/climate_data/gapfilled_climate_data_Kenya.csv", row.names = F)
