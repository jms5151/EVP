# climate plots ------------------------------------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(ggplot2)
library(ggridges)

# load data
load("Concatenated_Data/climate_data/merged_climate_data.RData")

# order sites for plotting 
climateData$Site <- factor(climateData$Site, levels=c("Chulaimbo", "Huaquillas", "Kisumu", "Machala", "Msambweni", "Portovelo", "Ukunda", "Zaruma"))

# plot weather variables across sites ------------------------------------------------
ggplot(data=climateData, aes(x=Date, y = Temperature, group=Site)) + geom_line() + facet_wrap(~Site, scales="free_x", ncol=2) + theme_bw() + ylab("Temperature (C)")
ggplot(data=climateData, aes(x=Date, y = Two_week_rainfall, group=Site)) + geom_line() + facet_wrap(~Site, scales='free_x', ncol=2) + theme_bw() + ylab("Rainfall (mm)") 
ggplot(data=climateData, aes(x=Date, y = Humidity, group=Site)) + geom_line() + facet_wrap(~Site, scales='free_x', ncol=2) + theme_bw() + ylab("Relative humidity (%)")
ggplot(data=climateData, aes(x=Date, y = SVPD, group=Site)) + geom_line() + facet_wrap(~Site, scales='free_x', ncol=2) + theme_bw() + ylab("Saturation vapor pressure deficit (kPA)")

# reorder sites for plotting 
climateData$Site <- factor(climateData$Site, levels=c("Zaruma", "Portovelo", "Machala", "Huaquillas", "Ukunda", "Msambweni", "Kisumu", "Chulaimbo"))
climateData$Site <- factor(climateData$Site, levels=c( "Ukunda", "Msambweni", "Kisumu", "Chulaimbo", "Zaruma", "Portovelo", "Machala", "Huaquillas"))

# plot distributions by weather variable across sites --------------------------------
ggplot(climateData, aes(x = Temperature, y = Site)) + geom_density_ridges2() + theme_bw() + ylab("") + xlab("Temperature (C)") + guides(fill=FALSE)
ggplot(climateData, aes(x = Two_week_rainfall, y = Site)) + geom_density_ridges2() + theme_bw() + ylab("") + xlab("Monthly cumulative rainfall (mm)") + guides(fill=FALSE)
ggplot(climateData, aes(x = Humidity, y = Site)) + geom_density_ridges2() + theme_bw() + ylab("") + xlab("Relative humidity (%)")  + guides(fill=FALSE)
ggplot(climateData, aes(x = SVPD, y = Site)) + geom_density_ridges2() + theme_bw() + ylab("") + xlab("Saturation vapor pressure deficit (kPA)")  + guides(fill=FALSE)
