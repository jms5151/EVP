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
# 2016-03-10 - 2017-12-08 Temperature, humidity, rainfall (portovelo)
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

# Repace/remove flagged data
h00r$RH[(which(h00r$date == "2017-01-31")):(which(h00r$date == "2017-04-14"))] <- NA
mach$RH[1:(which(mach$date == "2017-12-10"))] <- NA
port1819 <- port1819[1:(which(port1819$date == '2018-10-10')),]

# merge all datasets by site
Huaquillas <- do.call(rbind, setNames(lapply(ls(pattern="^h"), function(x) get(x)), ls(pattern="^h")))
Machala <- do.call(rbind, setNames(lapply(ls(pattern="^m"), function(x) get(x)), ls(pattern="^m")))
Portovelo <- do.call(rbind, setNames(lapply(ls(pattern="^p"), function(x) get(x)), ls(pattern="^p")))
Zaruma <- do.call(rbind, setNames(lapply(ls(pattern="^z"), function(x) get(x)), ls(pattern="^z")))

# add column to each dataset with site name
Huaquillas$Site <- "Huaquillas"
Machala$Site <- "Machala"
Portovelo$Site <- "Portovelo"
Zaruma$Site <- "Zaruma"

# replace flagged values with NA
Portovelo$temp[Portovelo$temp < 20] <- NA
Zaruma$temp[Zaruma$temp < 20] <- NA
Zaruma$temp[Zaruma$temp > 32] <- NA

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

# for each day, calculate aggregated rainfall values for month prior
for (j in 1:length(sites)){
  wide_data[paste0(sites[j], "_Two_week_rainfall")] <- NA
  for (k in 14:nrow(wide_data)){
    wide_data[k,paste0(sites[j], "_Two_week_rainfall")] <- sum(wide_data[(k-13):k, paste0(sites[j], "_rain")])
  }  
} 

# calculate average value by day and site across all years
wide_data$Month_Day <- format(wide_data$date, "%m-%d")
ltm <- wide_data
ltm <- ddply(ltm, .(Month_Day), summarize
             , Huaquillas_temp_ltm = ifelse(all(is.na(Huaquillas_temp)), NA, mean(Huaquillas_temp, na.rm=T))
             , Machala_temp_ltm = ifelse(all(is.na(Machala_temp)), NA, mean(Machala_temp, na.rm=T))
             , Portovelo_temp_ltm = ifelse(all(is.na(Portovelo_temp)), NA, mean(Portovelo_temp, na.rm=T))
             , Zaruma_temp_ltm = ifelse(all(is.na(Zaruma_temp)), NA, mean(Zaruma_temp, na.rm=T))
             , Huaquillas_rain_ltm = ifelse(all(is.na(Huaquillas_Two_week_rainfall)), NA, mean(Huaquillas_Two_week_rainfall, na.rm=T))
             , Machala_rain_ltm = ifelse(all(is.na(Machala_Two_week_rainfall)), NA, mean(Machala_Two_week_rainfall, na.rm=T))
             , Portovelo_rain_ltm = ifelse(all(is.na(Portovelo_Two_week_rainfall)), NA, mean(Portovelo_Two_week_rainfall, na.rm=T))
             , Zaruma_rain_ltm = ifelse(all(is.na(Zaruma_Two_week_rainfall)), NA, mean(Zaruma_Two_week_rainfall, na.rm=T))
             , Huaquillas_rh_ltm = ifelse(all(is.na(Huaquillas_humidity)), NA, mean(Huaquillas_humidity, na.rm=T))
             , Machala_rh_ltm = ifelse(all(is.na(Machala_humidity)), NA, mean(Machala_humidity, na.rm=T))
             , Portovelo_rh_ltm = ifelse(all(is.na(Portovelo_humidity)), NA, mean(Portovelo_humidity, na.rm=T))
             , Zaruma_rh_ltm = ifelse(all(is.na(Zaruma_humidity)), NA, mean(Zaruma_humidity, na.rm=T)))

wide_data <- merge(wide_data, ltm, by="Month_Day")

# save pre-gapfilled weather data
# x<-wide_data[,c("date", "Machala_temp", "Huaquillas_temp", "Portovelo_temp", "Zaruma_temp", "Machala_Two_week_rainfall", "Huaquillas_Two_week_rainfall", "Portovelo_Two_week_rainfall", "Zaruma_Two_week_rainfall", "Machala_humidity", "Huaquillas_humidity", "Portovelo_humidity", "Zaruma_humidity")]
# write.csv(x, "Concatenated_Data/climate_data/pregapfilled_data_Ecuador.csv", row.names = F)

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
wide_data$Zaruma_Temperature <- ifelse(!is.na(wide_data$Zaruma_Temperature), wide_data$Zaruma_Temperature, wide_data$Zaruma_temp_ltm)

# fill in missing rainfall data -----------------------------------------------------------------
# Huaquillas
fill_ha_w_ma <- lm(Huaquillas_Two_week_rainfall ~ Machala_Two_week_rainfall, data = wide_data)
wide_data$Huaquillas_Two_week_rainfall <- ifelse(!is.na(wide_data$Huaquillas_Two_week_rainfall), wide_data$Huaquillas_Two_week_rainfall, round(coef(fill_ha_w_ma)[[1]] + coef(fill_ha_w_ma)[[2]] * wide_data$Machala_Two_week_rainfall, 1))
wide_data$Huaquillas_Two_week_rainfall <- ifelse(!is.na(wide_data$Huaquillas_Two_week_rainfall), wide_data$Huaquillas_Two_week_rainfall, wide_data$Huaquillas_rain_ltm)

# Machala
fill_ma_w_ha <- lm(Machala_Two_week_rainfall ~ Huaquillas_Two_week_rainfall, data = wide_data)
wide_data$Machala_Two_week_rainfall <- ifelse(!is.na(wide_data$Machala_Two_week_rainfall), wide_data$Machala_Two_week_rainfall, round(coef(fill_ma_w_ha)[[1]] + coef(fill_ma_w_ha)[[2]] * wide_data$Huaquillas_Two_week_rainfall, 1))
# wide_data$Machala_Two_week_rainfall <- ifelse(!is.na(wide_data$Machala_Two_week_rainfall), wide_data$Machala_Two_week_rainfall, wide_data$Machala_rain_ltm)

# Portovelo
fill_po_w_za <- lm(Portovelo_Two_week_rainfall ~ Zaruma_Two_week_rainfall, data = wide_data)
wide_data$Portovelo_Two_week_rainfall <- ifelse(!is.na(wide_data$Portovelo_Two_week_rainfall), wide_data$Portovelo_Two_week_rainfall, round(coef(fill_po_w_za)[[1]] + coef(fill_po_w_za)[[2]] * wide_data$Zaruma_Two_week_rainfall, 1))
wide_data$Portovelo_Two_week_rainfall <- ifelse(!is.na(wide_data$Portovelo_Two_week_rainfall), wide_data$Portovelo_Two_week_rainfall, wide_data$Portovelo_rain_ltm)

# Zaruma
fill_za_w_po <- lm(Zaruma_Two_week_rainfall ~ Portovelo_Two_week_rainfall, data = wide_data)
wide_data$Zaruma_Two_week_rainfall <- ifelse(!is.na(wide_data$Zaruma_Two_week_rainfall), wide_data$Zaruma_Two_week_rainfall, round(coef(fill_za_w_po)[[1]] + coef(fill_za_w_po)[[2]] * wide_data$Portovelo_Two_week_rainfall, 1))
# wide_data$Zaruma_Two_week_rainfall <- ifelse(!is.na(wide_data$Zaruma_Two_week_rainfall), wide_data$Zaruma_Two_week_rainfall, wide_data$Zaruma_rain_ltm)
# replace values below zero with zero (due to negative coefficient value in regression)
wide_data$Zaruma_Two_week_rainfall <- ifelse(wide_data$Zaruma_Two_week_rainfall < 0, 0, wide_data$Zaruma_Two_week_rainfall)

# fill in missing humidity data -----------------------------------------------------------------
# Huaquillas
fill_ha_w_ma <- lm(Huaquillas_humidity ~ Machala_humidity, data = wide_data)
wide_data$Huaquillas_Humidity <- ifelse(!is.na(wide_data$Huaquillas_humidity), wide_data$Huaquillas_humidity, round(coef(fill_ha_w_ma)[[1]] + coef(fill_ha_w_ma)[[2]] * wide_data$Machala_humidity, 1))
wide_data$Huaquillas_Humidity <- ifelse(!is.na(wide_data$Huaquillas_Humidity), wide_data$Huaquillas_Humidity, wide_data$Huaquillas_rh_ltm)

# Machala
fill_ma_w_ha <- lm(Machala_humidity ~ Huaquillas_humidity, data = wide_data)
wide_data$Machala_Humidity <- ifelse(!is.na(wide_data$Machala_humidity), wide_data$Machala_humidity, round(coef(fill_ma_w_ha)[[1]] + coef(fill_ma_w_ha)[[2]] * wide_data$Huaquillas_humidity, 1))
wide_data$Machala_Humidity <- ifelse(!is.na(wide_data$Machala_Humidity), wide_data$Machala_Humidity, wide_data$Machala_rh_ltm)

# Portovelo
fill_po_w_za <- lm(Portovelo_humidity ~ Zaruma_humidity, data = wide_data)
wide_data$Portovelo_Humidity <- ifelse(!is.na(wide_data$Portovelo_humidity), wide_data$Portovelo_humidity, round(coef(fill_po_w_za)[[1]] + coef(fill_po_w_za)[[2]] * wide_data$Zaruma_humidity, 1))
wide_data$Portovelo_Humidity <- ifelse(!is.na(wide_data$Portovelo_Humidity), wide_data$Portovelo_Humidity, wide_data$Portovelo_rh_ltm)
# fill in feb 29th's with feb 28
wide_data$Portovelo_Humidity <- ifelse(!is.na(wide_data$Portovelo_Humidity), wide_data$Portovelo_Humidity, mean(wide_data$Portovelo_Humidity[wide_data$Month_Day=='02-28']))

# Zaruma
fill_za_w_po <- lm(Zaruma_humidity ~ Portovelo_humidity, data = wide_data)
wide_data$Zaruma_Humidity <- ifelse(!is.na(wide_data$Zaruma_humidity), wide_data$Zaruma_humidity, round(coef(fill_za_w_po)[[1]] + coef(fill_za_w_po)[[2]] * wide_data$Portovelo_humidity, 1))
wide_data$Zaruma_Humidity <- ifelse(!is.na(wide_data$Zaruma_Humidity), wide_data$Zaruma_Humidity, wide_data$Zaruma_rh_ltm)

# merge data into long format and save -----------------------------------------------------------
wide_data <- wide_data[order(wide_data$date),]

for (j in 1:length(sites)){
  weatherdf <- wide_data[, c("date", paste0(sites[j], "_Temperature"), paste0(sites[j], "_Two_week_rainfall"), paste0(sites[j], "_Humidity"))]
  colnames(weatherdf) <- gsub(paste0(sites[j], "_"), "", colnames(weatherdf))
  weatherdf$Site <- paste0(toupper(substr(sites[j],1,1)), substr(sites[j],2,nchar(sites[j])))
  assign(sites[j], weatherdf)
}  

# merge data into long format and save -----------------------------------------------------------
weatherdf <- do.call(rbind, list(Huaquillas, Machala, Portovelo, Zaruma))
colnames(weatherdf)[1] <- "Date"
portDiff <- mean(weatherdf$Temperature[weatherdf$Site=="Portovelo"&weatherdf$Date<"2013-05-06"]) - mean(weatherdf$Temperature[weatherdf$Site=="Portovelo"&weatherdf$Date>"2013-05-06"])# update portovelo weather
weatherdf$Temperature <- ifelse(weatherdf$Site == "Portovelo" & weatherdf$Date > "2013-05-06", weatherdf$Temperature+portDiff, weatherdf$Temperature)
weatherdf[,c(2:4)] <- lapply(weatherdf[,c(2:4)], function(x) round(x,2))
write.csv(weatherdf, "Concatenated_Data/climate_data/gapfilled_climate_data_Ecuador.csv", row.names = F)


