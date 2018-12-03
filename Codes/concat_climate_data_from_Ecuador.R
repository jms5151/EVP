# concatenate climate data from Ecuador field sites ---------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(tidyverse)

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

write.csv(gapfilled_data, "Concatenated_Data/climate_data/gapfilled_climate_data_Ecuador.csv", row.names=F)
