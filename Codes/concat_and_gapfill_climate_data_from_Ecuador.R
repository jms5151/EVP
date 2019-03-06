# concatenate climate data from Ecuador field sites ---------------------------------------

# Huaquillas:
# 2000-01-01 - 2012-12-31 Temperature, rainfall
# 2013-03-20 - 2014-05-16 Temperature, humidity, rainfall
# 2014-05-27 - 2015-05-26 Temperature, humidity, rainfall
# 2016-01-01 - 2017-12-10 Temperature, humidity, rainfall
# 2016-06-15 - 2018-07-04 Temperature, humidity, rainfall
# 
# Machala:
# 1985-12-01 - 2016-12-31 Temperature, rainfall
# 2013-03-20 - 2014-05-16 Temperature, humidity, rainfall
# 2014-05-18 - 2015-05-17 Temperature, humidity, rainfall
# 2016-01-01 - 2018-09-16 Temperature, humidity, rainfall
# 
# Portovelo:
# 2000-01-01 - 2012-12-31 Temperature, rainfall
# 2013-03-20 - 2014-05-16 Temperature, humidity, rainfall
# 2014-05-18 - 2015-05-17 Temperature, humidity, rainfall
# 2016-03-10 - 2017-12-08 Temperature, humidity, rainfall
# 2016-06-21 - 2018-07-10 Temperature, humidity, rainfall
# 
# Zaruma:
# 2000-01-01 - 2012-12-31 Temperature, rainfall
# 2013-03-20 - 2014-05-16 Temperature, humidity, rainfall
# 2014-05-28 - 2015-05-27 Temperature, humidity, rainfall
# 2016-01-04 - 2017-12-07 Temperature, humidity, rainfall
# 2016-06-21 - 2018-05-01 Temperature, humidity, rainfall

rm(list=ls()) #remove previous variable assignments

# load libraries
library(tidyverse)
library(plyr)

# load and format 2000-2012 climate data (2000-2016 for Machala) ------------------------------------
h0012 <- read.csv("Ecuador/EVP_Ecuador_Data/climate_Huaquillas.csv", head=T)
m8516 <- read.csv("Ecuador/EVP_Ecuador_Data/climate_Machala.csv", head=T)
z0012 <- read.csv("Ecuador/EVP_Ecuador_Data/climate_Zaruma_Portobello.csv", head=T)

# adjust mean temp of -999 in Huaquillas by adding average difference between Tmin and Tmean to Tmin
h0012$Tmean <- ifelse(h0012$Tmean < 0, h0012$Tmin + 1.4, h0012$Tmean)

# calculate mean temperature for Machala
m8516$Tmean <- (m8516$Tmax+m8516$Tmin)/2

# format dates and column names
clim.b4.2016 <- list(h0012, m8516, z0012)
clim.b4.2016.names <- c("h0012", "m8516", "z0012")

for (i in 1:length(clim.b4.2016)){
  clim1 <- clim.b4.2016[[i]]
  clim1$date <- as.Date(paste(clim1$Yr, clim1$Mo, clim1$Day, sep="-"), "%Y-%m-%d")
  clim1$RH <- NA
  clim1 <- clim1[,c("date", "Tmean", "RH", "RR")]
  colnames(clim1) <- c("date", "temp", "RH", "precip")
  assign(clim.b4.2016.names[i], clim1)
}

# subset Machala data by date
m8516 <- subset(m8516, date >= "2000-01-01" & date < "2013-03-20")

# load 2013-2018 climate data ----------------------------------------------------------------------
ecuador.climate.files <- list.files("Ecuador/EVP_Ecuador_Data/")
ecuador.climate.files <- ecuador.climate.files[grepl(".RData", ecuador.climate.files)]

for (j in 1:length(ecuador.climate.files)){
  fileName <- paste0("Ecuador/EVP_Ecuador_Data/", ecuador.climate.files[j])
  load(fileName)
}

# remove duplicated dates 
h00r <- h00r[!h00r$date %in% huaquillas$date,]
p00r <- p00r[!p00r$date %in% portovelo$date,]
z00r <- z00r[!z00r$date %in% zaruma$date,]

# combine all data by site
Huaquillas <- do.call(rbind, list(h00r, h0012, h13r, h14r, huaquillas))
Machala <- do.call(rbind, list(m8516, m13r, m14r, machala))
Portovelo <- do.call(rbind, list(p00r, p13r, p14r, portovelo))
Zaruma <- do.call(rbind, list(z00r, z0012, z13r, z14r, zaruma))

# change column names
ecuador.climate <- list(Huaquillas, Machala, Portovelo, Zaruma)
ecuador.sites <- c("Huaquillas", "Machala", "Portovelo", "Zaruma")

for (k in 1:length(ecuador.climate)){
  tempdf <- ecuador.climate[[k]]
  colnames(tempdf) <- c("Date", paste0("GF_", ecuador.sites[k], "_mean_temp"), paste0("GF_", ecuador.sites[k], "_humidity"), paste0("GF_", ecuador.sites[k], "_rain"))
  assign(ecuador.sites[k], tempdf)
}

# merge data -------------------------------------------------------------------------------------
gapfilled_data <- list(Huaquillas, Machala, Portovelo, Zaruma) %>% reduce(full_join, by = "Date")

# make all climate columns numeric
colsToNum <- colnames(gapfilled_data)[grepl("GF_", names(gapfilled_data))]
gapfilled_data[,colsToNum] <- lapply(gapfilled_data[,colsToNum], as.numeric)

# make sure all dates are included included
alldates <- as.data.frame(seq.Date(min(gapfilled_data$Date), max(gapfilled_data$Date), by="day"))
colnames(alldates) <- "Date"
gapfilled_data <- merge(alldates, gapfilled_data, by="Date", all = T)

# Gapfill data -----------------------------------------------------------------------------------
# calculate regression equations for temperature
fill.port.w.zar.temp = lm(GF_Portovelo_mean_temp ~ GF_Zaruma_mean_temp, data=gapfilled_data)
fill.zar.w.port.temp = lm(GF_Zaruma_mean_temp ~ GF_Portovelo_mean_temp, data=gapfilled_data)
fill.mach.w.huq.temp = lm(GF_Machala_mean_temp ~ GF_Huaquillas_mean_temp, data=gapfilled_data)
fill.huq.w.mach.temp = lm(GF_Huaquillas_mean_temp ~ GF_Machala_mean_temp, data=gapfilled_data)

# calculate regression equations for humidity
fill.port.w.zar.hum = lm(GF_Portovelo_humidity ~ GF_Zaruma_humidity, data=gapfilled_data)
fill.zar.w.port.hum = lm(GF_Zaruma_humidity ~ GF_Portovelo_humidity, data=gapfilled_data)
fill.mach.w.huq.hum = lm(GF_Machala_humidity ~ GF_Huaquillas_humidity, data=gapfilled_data)
fill.huq.w.mach.hum = lm(GF_Huaquillas_humidity ~ GF_Machala_humidity, data=gapfilled_data)

# calculate regression equations for humidity
fill.port.w.zar.rain = lm(GF_Portovelo_rain ~ GF_Zaruma_rain, data=gapfilled_data)
fill.zar.w.port.rain = lm(GF_Zaruma_rain ~ GF_Portovelo_rain, data=gapfilled_data)
fill.mach.w.huq.rain = lm(GF_Machala_rain ~ GF_Huaquillas_rain, data=gapfilled_data)
fill.huq.w.mach.rain = lm(GF_Huaquillas_rain ~ GF_Machala_rain, data=gapfilled_data)

# plot climate relationships
# plot(gapfilled_data$GF_Zaruma_mean_temp, gapfilled_data$GF_Portovelo_mean_temp, pch=16, xlab="Zaruma", ylab="Portovelo", main="Mean temperature", xlim=c(18,29), ylim=c(18,29))
# abline(0,1)
# abline(fill.port.w.zar.temp, col='blue', lwd=2)
# 
# plot(gapfilled_data$GF_Zaruma_humidity, gapfilled_data$GF_Portovelo_humidity, pch=16, xlab="Zaruma", ylab="Portovelo", main="Humidity", xlim=c(50,100), ylim=c(50,100))
# abline(0,1)
# abline(fill.port.w.zar.hum, col='blue', lwd=2)
# 
# plot(gapfilled_data$GF_Zaruma_rain, gapfilled_data$GF_Portovelo_rain, pch=16, xlab="Portovelo", ylab="Zaruma", main="Daily rainfall", xlim=c(0,25), ylim=c(0,25))
# abline(0,1)
# abline(fill.port.w.zar.rain, col='blue', lwd=2)
# 
# plot(gapfilled_data$GF_Machala_mean_temp, gapfilled_data$GF_Huaquillas_mean_temp, pch=16, xlab="Machala", ylab="Huaquillas", main="Mean temperature", xlim=c(20,31), ylim=c(20,31))
# abline(0,1)
# abline(fill.huq.w.mach.temp, col='blue', lwd=2)
# 
# plot(gapfilled_data$GF_Machala_humidity, gapfilled_data$GF_Huaquillas_humidity, pch=16, xlab="Machala", ylab="Huaquillas", main="Humidity", xlim=c(65,100), ylim=c(65,100))
# abline(0,1)
# abline(fill.huq.w.mach.hum, col='blue', lwd=2)
# 
# plot(gapfilled_data$GF_Machala_rain, gapfilled_data$GF_Huaquillas_rain, pch=16, xlab="Machala", ylab="Huaquillas", main="Daily rain", xlim=c(0,150), ylim=c(0,150))
# abline(0,1)
# abline(fill.huq.w.mach.rain, col='blue', lwd=2)

# gap fill temperature data
gapfilled_data$GF_Huaquillas_mean_temp <- ifelse(is.na(gapfilled_data$GF_Huaquillas_mean_temp), round(coef(fill.huq.w.mach.temp)[[1]] + coef(fill.huq.w.mach.temp)[[2]] * gapfilled_data$GF_Machala_mean_temp, 1), gapfilled_data$GF_Huaquillas_mean_temp)
gapfilled_data$GF_Machala_mean_temp <- ifelse(is.na(gapfilled_data$GF_Machala_mean_temp), round(coef(fill.mach.w.huq.temp)[[1]] + coef(fill.mach.w.huq.temp)[[2]] * gapfilled_data$GF_Huaquillas_mean_temp, 1), gapfilled_data$GF_Machala_mean_temp)
gapfilled_data$GF_Portovelo_mean_temp <- ifelse(is.na(gapfilled_data$GF_Portovelo_mean_temp), round(coef(fill.port.w.zar.temp)[[1]] + coef(fill.port.w.zar.temp)[[2]] * gapfilled_data$GF_Zaruma_mean_temp, 1), gapfilled_data$GF_Portovelo_mean_temp)
gapfilled_data$GF_Zaruma_mean_temp <- ifelse(is.na(gapfilled_data$GF_Zaruma_mean_temp), round(coef(fill.zar.w.port.temp)[[1]] + coef(fill.zar.w.port.temp)[[2]] * gapfilled_data$GF_Zaruma_mean_temp, 1), gapfilled_data$GF_Zaruma_mean_temp)

# gap fill humidity data
gapfilled_data$GF_Huaquillas_humidity <- ifelse(is.na(gapfilled_data$GF_Huaquillas_humidity), round(coef(fill.huq.w.mach.hum)[[1]] + coef(fill.huq.w.mach.hum)[[2]] * gapfilled_data$GF_Machala_humidity, 1), gapfilled_data$GF_Huaquillas_humidity)
gapfilled_data$GF_Machala_humidity <- ifelse(is.na(gapfilled_data$GF_Machala_humidity), round(coef(fill.mach.w.huq.hum)[[1]] + coef(fill.mach.w.huq.hum)[[2]] * gapfilled_data$GF_Huaquillas_humidity, 1), gapfilled_data$GF_Machala_humidity)
gapfilled_data$GF_Portovelo_humidity <- ifelse(is.na(gapfilled_data$GF_Portovelo_humidity), round(coef(fill.port.w.zar.hum)[[1]] + coef(fill.port.w.zar.hum)[[2]] * gapfilled_data$GF_Zaruma_humidity, 1), gapfilled_data$GF_Portovelo_humidity)
gapfilled_data$GF_Zaruma_humidity <- ifelse(is.na(gapfilled_data$GF_Zaruma_humidity), round(coef(fill.zar.w.port.hum)[[1]] + coef(fill.zar.w.port.hum)[[2]] * gapfilled_data$GF_Zaruma_humidity, 1), gapfilled_data$GF_Zaruma_humidity)

# gap fill rainfall data
gapfilled_data$GF_Huaquillas_rain <- ifelse(is.na(gapfilled_data$GF_Huaquillas_rain), round(coef(fill.huq.w.mach.rain)[[1]] + coef(fill.huq.w.mach.rain)[[2]] * gapfilled_data$GF_Machala_rain, 1), gapfilled_data$GF_Huaquillas_rain)
gapfilled_data$GF_Machala_rain <- ifelse(is.na(gapfilled_data$GF_Machala_rain), round(coef(fill.mach.w.huq.rain)[[1]] + coef(fill.mach.w.huq.rain)[[2]] * gapfilled_data$GF_Huaquillas_rain, 1), gapfilled_data$GF_Machala_rain)
gapfilled_data$GF_Portovelo_rain <- ifelse(is.na(gapfilled_data$GF_Portovelo_rain), round(coef(fill.port.w.zar.rain)[[1]] + coef(fill.port.w.zar.rain)[[2]] * gapfilled_data$GF_Zaruma_rain, 1), gapfilled_data$GF_Portovelo_rain)
gapfilled_data$GF_Zaruma_rain <- ifelse(is.na(gapfilled_data$GF_Zaruma_rain), round(coef(fill.zar.w.port.rain)[[1]] + coef(fill.zar.w.port.rain)[[2]] * gapfilled_data$GF_Zaruma_rain, 1), gapfilled_data$GF_Zaruma_rain)

# gap fill additional missing data with long term average daily values by site and month
gapfilled_data$Month <- substr(gapfilled_data$Date, 6, 7)
meanValues  <- ddply(gapfilled_data, .(Month), summarize
                     , Huaquillas_RH = round(mean(GF_Huaquillas_humidity, na.rm=T))
                     , Machala_RH = round(mean(GF_Machala_humidity, na.rm=T))
                     , Zaruma_RH = round(mean(GF_Zaruma_humidity, na.rm=T))
                     , Huaquillas_T = round(mean(GF_Huaquillas_mean_temp, na.rm=T))
                     , Machala_T = round(mean(GF_Machala_mean_temp, na.rm=T))
                     , Portovelo_T = round(mean(GF_Zaruma_mean_temp, na.rm=T))
                     , Zaruma_T = round(mean(GF_Zaruma_mean_temp, na.rm=T))
                     , Huaquillas_R = round(mean(GF_Huaquillas_rain, na.rm=T))
                     , Machala_R = round(mean(GF_Machala_rain, na.rm=T))
                     , Portovelo_R = round(mean(GF_Zaruma_rain, na.rm=T))
                     , Zaruma_R = round(mean(GF_Zaruma_rain, na.rm=T)))

gapfilled_data <- merge(gapfilled_data, meanValues, by="Month", all.x=T)

# temperature
gapfilled_data$GF_Huaquillas_mean_temp <- ifelse(is.na(gapfilled_data$GF_Huaquillas_mean_temp), gapfilled_data$Huaquillas_T, gapfilled_data$GF_Huaquillas_mean_temp)
gapfilled_data$GF_Machala_mean_temp <- ifelse(is.na(gapfilled_data$GF_Machala_mean_temp), gapfilled_data$Machala_T, gapfilled_data$GF_Machala_mean_temp)
gapfilled_data$GF_Portovelo_mean_temp <- ifelse(is.na(gapfilled_data$GF_Portovelo_mean_temp), gapfilled_data$Portovelo_T, gapfilled_data$GF_Portovelo_mean_temp)
gapfilled_data$GF_Zaruma_mean_temp <- ifelse(is.na(gapfilled_data$GF_Zaruma_mean_temp), gapfilled_data$Zaruma_T, gapfilled_data$GF_Zaruma_mean_temp)

# humidity
gapfilled_data$GF_Huaquillas_humidity <- ifelse(is.na(gapfilled_data$GF_Huaquillas_humidity), gapfilled_data$Huaquillas_RH, gapfilled_data$GF_Huaquillas_humidity)
gapfilled_data$GF_Machala_humidity <- ifelse(is.na(gapfilled_data$GF_Machala_humidity), gapfilled_data$Machala_RH, gapfilled_data$GF_Machala_humidity)
gapfilled_data$GF_Portovelo_humidity <- ifelse(is.na(gapfilled_data$GF_Portovelo_humidity), gapfilled_data$Zaruma_RH, gapfilled_data$GF_Portovelo_humidity)
gapfilled_data$GF_Zaruma_humidity <- ifelse(is.na(gapfilled_data$GF_Zaruma_humidity), gapfilled_data$Zaruma_RH, gapfilled_data$GF_Zaruma_humidity)

# rainfall
gapfilled_data$GF_Huaquillas_rain <- ifelse(is.na(gapfilled_data$GF_Huaquillas_rain), gapfilled_data$Huaquillas_R, gapfilled_data$GF_Huaquillas_rain)
gapfilled_data$GF_Machala_rain <- ifelse(is.na(gapfilled_data$GF_Machala_rain), gapfilled_data$Machala_R, gapfilled_data$GF_Machala_rain)
gapfilled_data$GF_Portovelo_rain <- ifelse(is.na(gapfilled_data$GF_Portovelo_rain), gapfilled_data$Zaruma_R, gapfilled_data$GF_Portovelo_rain)
gapfilled_data$GF_Zaruma_rain <- ifelse(is.na(gapfilled_data$GF_Zaruma_rain), gapfilled_data$Zaruma_R, gapfilled_data$GF_Zaruma_rain)

# create cumulative rainfall in prior week for each day
gapfilled_data <- gapfilled_data[order(gapfilled_data$Date),]

gapfilled_data$GF_Huaquillas_cumRain <- NA
gapfilled_data$GF_Machala_cumRain <- NA
gapfilled_data$GF_Portovelo_cumRain <- NA
gapfilled_data$GF_Zaruma_cumRain <- NA

for (n in 7:nrow(gapfilled_data)){
  rainSub <- subset(gapfilled_data, Date >= Date[n] - 6 & Date <= Date[n])
  gapfilled_data$GF_Huaquillas_cumRain[n] <- sum(rainSub$GF_Huaquillas_rain)
  gapfilled_data$GF_Machala_cumRain[n] <- sum(rainSub$GF_Machala_rain)
  gapfilled_data$GF_Portovelo_cumRain[n] <- sum(rainSub$GF_Portovelo_rain)
  gapfilled_data$GF_Zaruma_cumRain[n] <- sum(rainSub$GF_Zaruma_rain)
}

# create number of rainy days > 1mm/day in prior month for each day
gapfilled_data$GF_Huaquillas_rainyDays <- NA
gapfilled_data$GF_Machala_rainyDays <- NA
gapfilled_data$GF_Portovelo_rainyDays <- NA
gapfilled_data$GF_Zaruma_rainyDays <- NA

for (o in 30:nrow(gapfilled_data)){
  rainSub <- subset(gapfilled_data, Date >= Date[o] - 29 & Date <= Date[o])
  gapfilled_data$GF_Huaquillas_rainyDays[o] <- sum(rainSub$GF_Huaquillas_rain > 1)
  gapfilled_data$GF_Machala_rainyDays[o] <- sum(rainSub$GF_Machala_rain > 1)
  gapfilled_data$GF_Portovelo_rainyDays[o] <- sum(rainSub$GF_Portovelo_rain > 1)
  gapfilled_data$GF_Zaruma_rainyDays[o] <- sum(rainSub$GF_Zaruma_rain > 1)
}

colsToKeep <- colnames(gapfilled_data)[!grepl("_T|_RH|_R|Month", names(gapfilled_data))]
GF_data <- gapfilled_data[,colsToKeep]

# save data
write.csv(GF_data, "Concatenated_Data/climate_data/gapfilled_climate_data_Ecuador_2000-2018.csv", row.names=F)
