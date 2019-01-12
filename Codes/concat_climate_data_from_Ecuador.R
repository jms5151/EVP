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
write.csv(gapfilled_data, "Concatenated_Data/climate_data/gapfilled_climate_data_Ecuador.csv", row.names=F)

# format climate from 2003-2011 -------------------------------------------------------
# load data
clim.huaquillas <- read.csv("Ecuador/EVP_Ecuador_Data/climate_Huaquillas.csv", head=T)
clim.machala <- read.csv("Ecuador/EVP_Ecuador_Data/climate_Machala.csv", head=T)
clim.zarport <- read.csv("Ecuador/EVP_Ecuador_Data/climate_Zaruma_Portobello.csv", head=T)

# add site names
clim.huaquillas$Site <- "Huaquillas"
clim.machala$Site <- "Machala"
clim.zarport$Site <- "Portovelo.Zaruma"

# calculate mean temperature for Machala
clim.machala$Tmean <- (clim.machala$Tmax+clim.machala$Tmin)/2

# combine data and format
clim0311 <- do.call("rbind", list(clim.machala[,c("Site", "Yr", "Mo", "Day", "Tmean", "RR")], clim.huaquillas[,c("Site", "Yr", "Mo", "Day", "Tmean", "RR")], clim.zarport[,c("Site", "Yr", "Mo", "Day", "Tmean", "RR")])) 
clim0311$Date <- paste(clim0311$Yr, clim0311$Mo, clim0311$Day, sep='-')
clim0311$Date <- as.Date(clim0311$Date, "%Y-%m-%d")
clim0311 <- subset(clim0311, Date >= "2002-01-01" & Date <= "2011-12-31")

# create monthly averages for relative humidity by site
huaquillas$Site <- "Huaquillas"
machala$Site <- "Machala"
portovelo$Site <- "Portovelo.Zaruma"
zaruma$Site <- "Portovelo.Zaruma"

clim1618 <- do.call("rbind", list(huaquillas, machala, portovelo, zaruma))
clim1618$Mo <- substr(clim1618$date, 6,7) 
clim1618 <- ddply(clim1618, .(Site, Mo), summarize, RH = mean(RH, na.rm=T))

clim0311 <- merge(clim0311, clim1618, by=c("Site", "Mo"))

# create cumulative rainfall in prior week for each day
# gapfilled_data$GF_Huaquillas_cumRain <- NA
# gapfilled_data$GF_Machala_cumRain <- NA
# gapfilled_data$GF_Portovelo_cumRain <- NA
# gapfilled_data$GF_Zaruma_cumRain <- NA
# 
# for (j in 7:nrow(gapfilled_data)){
#   rainSub <- subset(gapfilled_data, Date >= Date[j] - 6 & Date <= Date[j])
#   gapfilled_data$GF_Huaquillas_cumRain[j] <- sum(rainSub$GF_Huaquillas_rain)
#   gapfilled_data$GF_Machala_cumRain[j] <- sum(rainSub$GF_Machala_rain)
#   gapfilled_data$GF_Portovelo_cumRain[j] <- sum(rainSub$GF_Portovelo_rain)
#   gapfilled_data$GF_Zaruma_cumRain[j] <- sum(rainSub$GF_Zaruma_rain)
# }

# subset data
clim0311 <- clim0311[,c("Date", "Tmean", "RR", "RH")]
colnames(clim0311) <- c("Date", "mean_temp", "rainfall", "humidity")

# save data
write.csv(clim0311, "Concatenated_Data/climate_data/climate_data_Ecuador_2003-2011.csv", row.names=F)

# plot Zaruma and Portovelo relionship for adjusting older temperature data
# port_zaru <- lm(GF_Zaruma_mean_temp~GF_Portovelo_mean_temp, data=gapfilled_data)
# 
# plot(gapfilled_data$GF_Portovelo_mean_temp, gapfilled_data$GF_Zaruma_mean_temp, pch=16, ylim=c(16,29), xlim=c(16,29), ylab='Zaruma mean temperature', xlab='Portovelo mean temperature')
# abline(port_zaru, col='blue')
# abline(0,1)
# legend("topleft", legend = c('1:1 line', 'Regression line'), lty=c(1,1), col=c('black', 'blue'), text.col=c('black', 'blue'), bty='n')

# plot(gapfilled_data$Date, gapfilled_data$GF_Huaquillas_humidity, ylab='Humidity', xlab = 'Date', type='l', ylim=c(60,100))
# lines(gapfilled_data$Date, gapfilled_data$GF_Machala_humidity, col='darkred')
# legend("bottomright", legend=c('Huaquillas', 'Machala'), lty=c(1,1), col=c('black', 'darkred'), text.col=c('black', 'darkred'), bty='n')
# plot(gapfilled_data$Date, gapfilled_data$GF_Zaruma_humidity, col='darkblue', ylab='Humidity', xlab = 'Date', type='l', ylim=c(60,100))
# lines(gapfilled_data$Date, gapfilled_data$GF_Portovelo_humidity, col='darkgreen')
# legend("bottomleft", legend=c('Zaruma', 'Portovelo'), lty=c(1,1), col=c('darkblue', 'darkgreen'), text.col=c('darkblue', 'darkgreen'), bty='n')

