# concatenate climate data from Ecuador field sites ---------------------------------------
# Huaquillas:
# 2000-01-01 - 2012-12-31 Temperature, rainfall (h0012 - CSV)
# 2013-03-20 - 2014-05-16 Temperature, humidity, rainfall (h14r)
# 2014-05-27 - 2015-05-26 Temperature, humidity, rainfall (h13r)
# 2016-01-01 - 2017-12-10 Temperature, humidity, rainfall (huaquillas)
# 2016-06-15 - 2018-07-04 Temperature, humidity, rainfall (h00r)
# 2018-04-21 - 2019-01-07 Temperature, humidity, rainfall (huaq1819)
# 
# Machala:
# 1985-12-01 - 2016-12-31 Temperature, rainfall (m8516)
# 2013-03-20 - 2014-05-16 Temperature, humidity, rainfall (m14r)
# 2014-05-18 - 2015-05-17 Temperature, humidity, rainfall (m13r)
# 2016-01-01 - 2017-12-10 Temperature, humidity, rainfall (machala)
# 2016-08-28 - 2018-09-16 Temperature, humidity, rainfall (mach)
# 2018-06-27 - 2019-02-12 Temperature, humidity, rainfall (mach1819)
# 
# Portovelo:
# 2000-01-01 - 2012-12-31 Temperature, rainfall (z0012)
# 2013-03-20 - 2014-05-16 Temperature, humidity, rainfall (p14r)
# 2014-05-18 - 2015-05-17 Temperature, humidity, rainfall (p13r)
# 2016-03-10 - 2017-12-08 Temperature, humidity, rainfall (porotvelo)
# 2016-06-21 - 2018-07-10 Temperature, humidity, rainfall (p00r)
# 2018-04-23 - 2019-01-09 Temperature, humidity, rainfall (port1819)
# 
# Zaruma:
# 2000-01-01 - 2012-12-31 Temperature, rainfall (z0012)
# 2013-03-20 - 2014-05-16 Temperature, humidity, rainfall (z14r)
# 2014-05-28 - 2015-05-27 Temperature, humidity, rainfall (z13r)
# 2016-01-04 - 2017-12-07 Temperature, humidity, rainfall (zaruma)
# 2016-06-21 - 2018-05-01 Temperature, humidity, rainfall (z00r)
# 2018-04-23 - 2019-01-19 Temperature, humidity, rainfall (zar1819)

rm(list=ls()) #remove previous variable assignments

# load libraries
library(tidyverse)
library(plyr)

# list files
ecuador.climate.files <- list.files("Ecuador/EVP_Ecuador_Data/climate_data/")

# load and format weather data ----------------------------------------
# load csv files
ecuador.csv.files <- ecuador.climate.files[grepl(".csv", ecuador.climate.files)]
csvnames <- c("h0012", "m8516", "z0012")

for (i in 1:length(ecuador.csv.files)){
  fileName <- paste0("Ecuador/EVP_Ecuador_Data/climate_data/", ecuador.csv.files[i])
  x <- read.csv(fileName, head=T)
  x$date <- as.Date(paste(x$Yr, x$Mo, x$Day, sep="-"), "%Y-%m-%d") #as.Date(strptime(x$Date, "%Y%m%d"), "%Y-%m-%d")
  x$RH <- NA
  if (csvnames[i] == "h0012"){
    # adjust mean temp of -999 in Huaquillas by adding average difference between Tmin and Tmean to Tmin
    x$Tmean <- ifelse(x$Tmean < 0, x$Tmin + 1.4, x$Tmean)
  }
  if (csvnames[i] == "m8516"){
    # calculate mean temperature for Machala
    x$Tmean <- (x$Tmax+x$Tmin)/2
  }
  x <- x[,c("date", "Tmean", "RH", "RR")]
  colnames(x) <- c("date", "temp", "RH", "precip")
  assign(csvnames[i], x)
}

# load rdata files
ecuador.rdata.climate <- ecuador.climate.files[grepl(".RData", ecuador.climate.files)]

for (j in 1:length(ecuador.rdata.climate)){
  fileName <- paste0("Ecuador/EVP_Ecuador_Data/climate_data/", ecuador.rdata.climate[j])
  load(fileName)
}

# load and format NOAA GSOD (Global Summary of the Day) from 
# https://www7.ncdc.noaa.gov/CDO/cdoselect.cmd?datasetabbv=GSOD&countryabbv&georegionabbv
# ecuador.txt.climate <- ecuador.climate.files[grepl(".txt", ecuador.climate.files)]
# txtName <- c("GSOD_Atahualpa", "GSOD_Loja", "GSOD_Santa_Rosa_Aeropuerto")
# 
# for (k in 1:length(ecuador.txt.climate)){
#   fileName <- paste0("Ecuador/EVP_Ecuador_Data/climate_data/", ecuador.txt.climate[k])
#   x <- read.delim(fileName, head=TRUE, sep=',')
#   x$mean_temp_gsod <- round((x$TEMP-32)*(5/9), 2)
#   x$Date = as.Date(paste(substr(x$YEARMODA, 1, 4), substr(x$YEARMODA, 5, 6), substr(x$YEARMODA, 7, 8), sep="-"), "%Y-%m-%d")
#   x$rain_gsod <- as.numeric(gsub("[A-Z]|99.99", "", x$PRCP)) 
#   x$rain_gsod <- round((x$rain_gsod * 25.4), 2)
#   x <- x[,c("Date", "mean_temp_gsod", "rain_gsod")]
#   assign(txtName[k],x)
# }

# format temperature and site
Huaquillas <- do.call(rbind, setNames(lapply(ls(pattern="^h"), function(x) get(x)), ls(pattern="^h")))
Machala <- do.call(rbind, setNames(lapply(ls(pattern="^m"), function(x) get(x)), ls(pattern="^m")))
Portovelo <- do.call(rbind, setNames(lapply(ls(pattern="^p"), function(x) get(x)), ls(pattern="^p")))
Zaruma <- do.call(rbind, setNames(lapply(ls(pattern="^z"), function(x) get(x)), ls(pattern="^z")))

# add column with site name
Huaquillas$Site <- "Huaquillas"
Machala$Site <- "Machala"
Portovelo$Site <- "Portovelo"
Zaruma$Site <- "Zaruma"

# replace suspect temperature & humidity values with NA (data issue for Portovelo, ~90 dates with temp of 0 degrees and 1 with 16 degrees)
Portovelo$temp <- ifelse(Portovelo$temp < 17, NA, Portovelo$temp)
Zaruma$temp <- ifelse(Zaruma$temp > 30, NA, Zaruma$temp)

Huaquillas$RH <- ifelse(Huaquillas$RH < 48, NA, Huaquillas$RH)
Machala$RH <- ifelse(Machala$RH < 48, NA, Machala$RH)
Portovelo$RH <- ifelse(Portovelo$RH < 48, NA, Portovelo$RH)
Zaruma$RH <- ifelse(Zaruma$RH < 48, NA, Zaruma$RH)

# combine all data
weather <- do.call(rbind, list(Huaquillas, Machala, Portovelo, Zaruma))

# average data for each date (some datasets overlap)
weather2 <- ddply(weather, .(date, Site), summarize
                 , temp = ifelse(all(is.na(temp))==FALSE, mean(temp, na.rm=T), NA) 
                 , rain = ifelse(all(is.na(precip))==FALSE, mean(precip, na.rm=T), NA)
                 , humidity = ifelse(all(is.na(RH))==FALSE, mean(RH, na.rm=T), NA))

# reshape from long to wide, making sure all dates are included between minDate and maxDate
minDate <- as.Date('2001-01-01', '%Y-%m-%d')
maxDate <- as.Date('2019-02-01', '%Y-%m-%d')
wide_data <- as.data.frame(seq.Date(minDate, maxDate, by=1))
colnames(wide_data) <- "date"
sites <- unique(weather2$Site)

for (l in 1:length(sites)){
  x <- subset(weather2, Site == sites[l])
  x <- x[, !names(x) %in% c("Site")]
  colnames(x)[2:4] <- paste(sites[l], colnames(x)[2:4], sep='_')
  wide_data <- merge(wide_data, x, by="date", all.x=T)
}

# calculate average value by day and site across all years
wide_data$Month_Day <- format(wide_data$date, "%m-%d")
ltm <- wide_data
ltm <- ddply(ltm, .(Month_Day), summarize
             , Huaquillas_temp_ltm = ifelse(all(is.na(Huaquillas_temp)), NA, mean(Huaquillas_temp, na.rm=T))
             , Machala_temp_ltm = ifelse(all(is.na(Machala_temp)), NA, mean(Machala_temp, na.rm=T))
             , Portovelo_temp_ltm = ifelse(all(is.na(Portovelo_temp)), NA, mean(Portovelo_temp, na.rm=T))
             , Zaruma_temp_ltm = ifelse(all(is.na(Zaruma_temp)), NA, mean(Zaruma_temp, na.rm=T))
             , Huaquillas_rain_ltm = ifelse(all(is.na(Huaquillas_rain)), NA, mean(Huaquillas_rain, na.rm=T))
             , Machala_rain_ltm = ifelse(all(is.na(Machala_rain)), NA, mean(Machala_rain, na.rm=T))
             , Portovelo_rain_ltm = ifelse(all(is.na(Portovelo_rain)), NA, mean(Portovelo_rain, na.rm=T))
             , Zaruma_rain_ltm = ifelse(all(is.na(Zaruma_rain)), NA, mean(Zaruma_rain, na.rm=T))
             , Huaquillas_rh_ltm = ifelse(all(is.na(Huaquillas_humidity)), NA, mean(Huaquillas_humidity, na.rm=T))
             , Machala_rh_ltm = ifelse(all(is.na(Machala_humidity)), NA, mean(Machala_humidity, na.rm=T))
             , Portovelo_rh_ltm = ifelse(all(is.na(Portovelo_humidity)), NA, mean(Portovelo_humidity, na.rm=T))
             , Zaruma_rh_ltm = ifelse(all(is.na(Zaruma_humidity)), NA, mean(Zaruma_humidity, na.rm=T)))

wide_data <- merge(wide_data, ltm, by="Month_Day")

# fill in missing temperature data -----------------------------------------------------------------
# Huaquillas
fill_ha_w_ma <- lm(Huaquillas_temp ~ Machala_temp, data = wide_data)
wide_data$Huaquillas_Temperature <- ifelse(!is.na(wide_data$Huaquillas_temp), wide_data$Huaquillas_temp, round(coef(fill_ha_w_ma)[[1]] + coef(fill_ha_w_ma)[[2]] * wide_data$Machala_temp, 1))
wide_data$Huaquillas_Temperature <- ifelse(!is.na(wide_data$Huaquillas_Temperature), wide_data$Huaquillas_Temperature, wide_data$Huaquillas_temp_ltm)

# Machala
fill_ma_w_ha <- lm(Machala_temp ~ Huaquillas_temp, data = wide_data)
wide_data$Machala_Temperature <- ifelse(!is.na(wide_data$Machala_temp), wide_data$Machala_temp, round(coef(fill_ma_w_ha)[[1]] + coef(fill_ma_w_ha)[[2]] * wide_data$Huaquillas_temp, 1))
wide_data$Machala_Temperature <- ifelse(!is.na(wide_data$Machala_Temperature), wide_data$Machala_Temperature, wide_data$Machala_temp_ltm)

# Portovelo
fill_po_w_za <- lm(Portovelo_temp ~ Zaruma_temp, data = wide_data)
wide_data$Portovelo_Temperature <- ifelse(!is.na(wide_data$Portovelo_temp), wide_data$Portovelo_temp, round(coef(fill_po_w_za)[[1]] + coef(fill_po_w_za)[[2]] * wide_data$Zaruma_temp, 1))
wide_data$Portovelo_Temperature <- ifelse(!is.na(wide_data$Portovelo_Temperature), wide_data$Portovelo_Temperature, wide_data$Portovelo_temp_ltm)

# Zaruma
fill_za_w_po <- lm(Zaruma_temp ~ Portovelo_temp, data = wide_data)
wide_data$Zaruma_Temperature <- ifelse(!is.na(wide_data$Zaruma_temp), wide_data$Zaruma_temp, round(coef(fill_za_w_po)[[1]] + coef(fill_za_w_po)[[2]] * wide_data$Portovelo_temp, 1))
wide_data$Zaruma_Temperature <- ifelse(!is.na(wide_data$Zaruma_Temperature), wide_data$Zaruma_Temperature, wide_data$Portovelo_temp_ltm)

# fill in missing rainfall data -----------------------------------------------------------------
wide_data$Huaquillas_Rainfall <- ifelse(!is.na(wide_data$Huaquillas_rain), wide_data$Huaquillas_rain, wide_data$Huaquillas_rain_ltm)
wide_data$Machala_Rainfall <- ifelse(!is.na(wide_data$Machala_rain), wide_data$Machala_rain, wide_data$Machala_rain_ltm)
wide_data$Portovelo_Rainfall <- ifelse(!is.na(wide_data$Portovelo_rain), wide_data$Portovelo_rain, wide_data$Portovelo_rain_ltm)
# fill in feb 29th's with feb 28
wide_data$Portovelo_Rainfall <- ifelse(!is.na(wide_data$Portovelo_Rainfall), wide_data$Portovelo_Rainfall, mean(wide_data$Portovelo_Rainfall[wide_data$Month_Day=='02-28']))
wide_data$Zaruma_Rainfall <- ifelse(!is.na(wide_data$Zaruma_rain), wide_data$Zaruma_rain, wide_data$Zaruma_rain_ltm)

# fill in missing humidity data -----------------------------------------------------------------
wide_data$Huaquillas_Humidity <- ifelse(!is.na(wide_data$Huaquillas_humidity), wide_data$Huaquillas_humidity, wide_data$Huaquillas_rh_ltm)
wide_data$Machala_Humidity <- ifelse(!is.na(wide_data$Machala_humidity), wide_data$Machala_humidity, wide_data$Machala_rh_ltm)
wide_data$Portovelo_Humidity <- ifelse(!is.na(wide_data$Portovelo_humidity), wide_data$Portovelo_humidity, wide_data$Portovelo_rh_ltm) # still four days missing
# fill in feb 29th's with feb 28
wide_data$Portovelo_Humidity <- ifelse(!is.na(wide_data$Portovelo_Humidity), wide_data$Portovelo_Humidity, mean(wide_data$Portovelo_Humidity[wide_data$Month_Day=='02-28']))
wide_data$Zaruma_Humidity <- ifelse(!is.na(wide_data$Zaruma_humidity), wide_data$Zaruma_humidity, wide_data$Zaruma_rh_ltm)

# calculate 30 day aggregated rainfall values ---------------------------------------------------
wide_data <- wide_data[order(wide_data$date),]

for (m in 1:length(sites)){
  weatherdf <- wide_data[, c("date", paste0(sites[m], "_Temperature"), paste0(sites[m], "_Rainfall"), paste0(sites[m], "_Humidity"))]
  weatherdf <- weatherdf[order(weatherdf$date),]
  weatherdf$Monthly_rainfall <- NA
  weatherdf$Monthly_rainfall_weighted <- NA
  weatherdf$Monthly_rainy_days_25 <- NA
  for (n in 30:nrow(weatherdf)){
    rainSub <- subset(weatherdf, date >= date[n] - 29 & date <= date[n])
    rainSub$exDec <- 30:1
    rainSub$exDec <- rainSub[,paste0(sites[m], "_Rainfall")] * (1/rainSub$exDec) 
    weatherdf$Monthly_rainfall[n] <- sum(rainSub[paste0(sites[m], "_Rainfall")])
    weatherdf$Monthly_rainfall_weighted[n] <- sum(rainSub$exDec)
    weatherdf$Monthly_rainy_days_25[n] <- sum(rainSub[paste0(sites[m], "_Rainfall")] > 2.5)
  }
  colnames(weatherdf) <- gsub(paste0(sites[m], "_"), "", colnames(weatherdf))
  weatherdf$Site <- sites[m]
  weatherdf <- weatherdf[30:nrow(weatherdf),]
  assign(sites[m], weatherdf)
}  

# merge data into long format and save -----------------------------------------------------------
weatherdf <- do.call(rbind, list(Huaquillas, Machala, Portovelo, Zaruma))
colnames(weatherdf)[1] <- "Date"
weatherdf[,c(2:7)] <- lapply(weatherdf[,c(2:7)], function(x) round(x,2))
write.csv(weatherdf, "Concatenated_Data/climate_data/gapfilled_climate_data_Ecuador.csv", row.names = F)
