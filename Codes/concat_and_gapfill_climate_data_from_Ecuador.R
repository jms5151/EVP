# concatenate climate data from Ecuador field sites ---------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(tidyverse)
library(plyr)

# load data
load("Ecuador/EVP_Ecuador_Data/huaquillas_full.RData")
load("Ecuador/EVP_Ecuador_Data/machala_full.RData")
load("Ecuador/EVP_Ecuador_Data/portovelo_full.RData")
load("Ecuador/EVP_Ecuador_Data/zaruma_full.RData")

# change column names
ecuador.climate <- list(huaquillas, machala, portovelo, zaruma)
ecuador.sites <- c("Huaquillas", "Machala", "Portovelo", "Zaruma")

for (i in 1:length(ecuador.climate)){
  tempdf <- ecuador.climate[[i]]
  colnames(tempdf) <- c("Date", paste0("GF_", ecuador.sites[i], "_mean_temp"), paste0("GF_", ecuador.sites[i], "_humidity"), paste0("GF_", ecuador.sites[i], "_rain"))
  assign(ecuador.sites[i], tempdf)
}

# merge data
gapfilled_data <- list(Huaquillas, Machala, Portovelo, Zaruma) %>% reduce(full_join, by = "Date")

# create cumulative rainfall in prior week for each day
gapfilled_data$GF_Huaquillas_cumRain <- NA
gapfilled_data$GF_Machala_cumRain <- NA
gapfilled_data$GF_Portovelo_cumRain <- NA
gapfilled_data$GF_Zaruma_cumRain <- NA

for (j in 7:nrow(gapfilled_data)){
  rainSub <- subset(gapfilled_data, Date >= Date[j] - 6 & Date <= Date[j])
  gapfilled_data$GF_Huaquillas_cumRain[j] <- sum(rainSub$GF_Huaquillas_rain)
  gapfilled_data$GF_Machala_cumRain[j] <- sum(rainSub$GF_Machala_rain)
  gapfilled_data$GF_Portovelo_cumRain[j] <- sum(rainSub$GF_Portovelo_rain)
  gapfilled_data$GF_Zaruma_cumRain[j] <- sum(rainSub$GF_Zaruma_rain)
}

# save data
write.csv(gapfilled_data, "Concatenated_Data/climate_data/gapfilled_climate_data_Ecuador_2016-2017.csv", row.names=F)

# format climate from 1985-2016 -------------------------------------------------------
# load data
clim.huaquillas <- read.csv("Ecuador/EVP_Ecuador_Data/climate_Huaquillas.csv", head=T)
clim.machala <- read.csv("Ecuador/EVP_Ecuador_Data/climate_Machala.csv", head=T)
clim.zaruma <- read.csv("Ecuador/EVP_Ecuador_Data/climate_Zaruma_Portobello.csv", head=T)

# adjust mean temp of -999 in Huaquillas by adding average difference between Tmin and Tmean to Tmin
clim.huaquillas$Tmean <- ifelse(clim.huaquillas$Tmean < 0, clim.huaquillas$Tmin + 1.4, clim.huaquillas$Tmean)

# reduce climate period for Machala
# clim.machala <- subset(clim.machala, Yr > 2000)

# calculate mean temperature for Machala
clim.machala$Tmean <- (clim.machala$Tmax+clim.machala$Tmin)/2
clim.machala$Tmean <- ifelse(!is.na(clim.machala$Tmean), clim.machala$Tmean, clim.machala$Tmed) # use median if Tmax and Tmin have missing values
clim.machala$Tmean <- ifelse(!is.na(clim.machala$Tmean), clim.machala$Tmean, clim.machala$Tmin + 3.47) # use min plus mean difference between Tmin and Tmean
clim.machala$Tmean <- ifelse(!is.na(clim.machala$Tmean), clim.machala$Tmean, clim.machala$Tmax - 3.50) # use max minus mean difference between Tmax and Tmean

for (i in 11:nrow(clim.machala)){
  if (is.na(clim.machala$Tmean[i])==TRUE & !is.na(clim.machala$Tmean[i-1])==TRUE & !is.na(clim.machala$Tmean[i+1])==TRUE){
    clim.machala$Tmean[i] <- mean(clim.machala$Tmean[i-1]:clim.machala$Tmean[i+1])
  }
}

# calculate average humidity values by month for 2016-2017 data
gapfilled_data <- read.csv("Concatenated_Data/climate_data/gapfilled_climate_data_Ecuador_2016-2017.csv", head=T, stringsAsFactors = F)
gapfilled_data$Month <- substr(gapfilled_data$Date, 6, 7)
meanHumidity <- ddply(gapfilled_data, .(Month), summarize, Huaquillas_RH = round(mean(GF_Huaquillas_humidity, na.rm=T)), Machala_RH = round(mean(GF_Machala_humidity, na.rm=T)), Zaruma_RH = round(mean(GF_Zaruma_humidity, na.rm=T)))

# combine data, add humidity by site, and change column names 
ecuador.climate2 <- list(clim.huaquillas, clim.machala, clim.zaruma)
ecuador.sites2 <- c("Huaquillas", "Machala", "Zaruma")

for (i in 1:length(ecuador.climate2)){
  # subset data
  tempdf <- ecuador.climate2[[i]]
  # format date
  tempdf$Date <- paste(tempdf$Yr, tempdf$Mo, tempdf$Day, sep='-')
  tempdf$Date <- as.Date(tempdf$Date, "%Y-%m-%d")
  tempdf$Month <- format(tempdf$Date, "%m")
  # add relative humidity
  rh <- paste0(ecuador.sites2[i], "_RH")
  tempdf <- merge(tempdf, meanHumidity[,c("Month", rh)], by="Month")
  # subset and rename columns
  tempdf <- tempdf[,c("Date", "Tmean", rh, "RR")]
  colnames(tempdf) <- c("Date", paste0("GF_", ecuador.sites2[i], "_mean_temp"), paste0("GF_", ecuador.sites2[i], "_humidity"), paste0("GF_", ecuador.sites2[i], "_rain"))
  assign(paste0(ecuador.sites2[i], "2"), tempdf)
}

# merge data
clim8516 <- list(Huaquillas2, Machala2, Zaruma2) %>% reduce(full_join, by = "Date")

# replace missing values for rain with zero
rainCols <- grep("rain", names(clim8516), value = TRUE)
clim8516[,rainCols][is.na(clim8516[,rainCols])] <- 0

# calculate regression equations to relate Zaruma climate to Portovelo climate and to gapfill for Machala from Huaquillas
fill.port.w.zar.temp = lm(GF_Portovelo_mean_temp ~ GF_Zaruma_mean_temp, data=gapfilled_data)
fill.port.w.zar.humidity = lm(GF_Portovelo_humidity ~ GF_Zaruma_humidity, data=gapfilled_data)
fill.port.w.zar.rain = lm(GF_Portovelo_rain ~ GF_Zaruma_rain, data=gapfilled_data)
fill.mach.w.huq.temp = lm(GF_Machala_mean_temp ~ GF_Huaquillas_mean_temp, data=gapfilled_data)

# plot relationships between weather in Portovelo and Zaruma
# plot(gapfilled_data$GF_Zaruma_humidity, gapfilled_data$GF_Portovelo_humidity, pch=16, xlab="Zaruma", ylab="Portovelo", main="Humidity", xlim=c(50,100), ylim=c(50,100))
# abline(0,1)
# abline(fill.port.w.zar.humidity, col='blue', lwd=2)
# 
# plot(gapfilled_data$GF_Zaruma_mean_temp, gapfilled_data$GF_Portovelo_mean_temp, pch=16, xlab="Zaruma", ylab="Portovelo", main="Mean temperature", xlim=c(18,29), ylim=c(18,29))
# abline(0,1)
# abline(fill.port.w.zar.temp, col='blue', lwd=2)
# 
# plot(gapfilled_data$GF_Zaruma_rain, gapfilled_data$GF_Portovelo_rain, pch=16, xlab="Portovelo", ylab="Zaruma", main="Daily rainfall", xlim=c(0,25), ylim=c(0,25))
# abline(0,1)
# abline(fill.port.w.zar.rain, col='blue', lwd=2)
# 
# plot(gapfilled_data$GF_Machala_mean_temp, gapfilled_data$GF_Huaquillas_mean_temp, pch=16, xlab="Machala", ylab="Huaquillas", main="Mean temperature", xlim=c(20,31), ylim=c(20,31))
# abline(0,1)
# abline(fill.mach.w.huq.temp, col='blue', lwd=2)

# gap fill Portovelo climate with Zaruma climate
clim8516$GF_Portovelo_mean_temp <- round(coef(fill.port.w.zar.temp)[[1]] + coef(fill.port.w.zar.temp)[[2]] * clim8516$GF_Zaruma_mean_temp, 1)
clim8516$GF_Portovelo_humidity <- round(coef(fill.port.w.zar.humidity)[[1]] + coef(fill.port.w.zar.humidity)[[2]] * clim8516$GF_Zaruma_humidity, 1)
clim8516$GF_Portovelo_rain <- round(coef(fill.port.w.zar.rain)[[1]] + coef(fill.port.w.zar.rain)[[2]] * clim8516$GF_Zaruma_rain, 1)
clim8516$GF_Machala_mean_temp <- ifelse(is.na(clim8516$GF_Machala_mean_temp), round(coef(fill.mach.w.huq.temp)[[1]] + coef(fill.mach.w.huq.temp)[[2]] * clim8516$GF_Huaquillas_mean_temp, 1), clim8516$GF_Machala_mean_temp)

# create cumulative rainfall in prior week for each day
clim8516$GF_Huaquillas_cumRain <- NA
clim8516$GF_Machala_cumRain <- NA
clim8516$GF_Portovelo_cumRain <- NA
clim8516$GF_Zaruma_cumRain <- NA

for (j in 7:nrow(clim8516)){
  rainSub <- subset(clim8516, Date >= Date[j] - 6 & Date <= Date[j])
  clim8516$GF_Huaquillas_cumRain[j] <- sum(rainSub$GF_Huaquillas_rain)
  clim8516$GF_Machala_cumRain[j] <- sum(rainSub$GF_Machala_rain)
  clim8516$GF_Portovelo_cumRain[j] <- sum(rainSub$GF_Portovelo_rain)
  clim8516$GF_Zaruma_cumRain[j] <- sum(rainSub$GF_Zaruma_rain)
}

# sort in ascending order
clim8516 <- clim8516[order(clim8516$Date),]

# reduce dataset to years with data
clim8516$Year <- substr(clim8516$Date, 1, 4)
clim0215 <- subset(clim8516, Year >= 2002 & Year <= 2015)
clim0215$Year <- NULL

# save data
write.csv(clim0215, "Concatenated_Data/climate_data/gapfilled_climate_data_Ecuador_2002-2015.csv", row.names=F)
