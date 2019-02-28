# climate plots ------------------------------------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(plotly)
library(reshape2)
library(ggplot2)
library(plyr)

# load data
climateData <- read.csv("Concatenated_Data/climate_data/merged_climate_data.csv", head=T)
climateData$Date <- as.Date(climateData$Date, "%Y-%m-%d")

# plot temp, rain, and humidity for a specific site -----------------------------------
temp <- plot_ly() %>%
  add_trace(data=climateData, x = ~Date, y = ~GF_Kisumu_mean_temp, type = 'scatter', mode = 'lines', color=I('darkred'))%>%
  layout(yaxis = list(side = 'left', title = 'Mean temperature (C)', showgrid = FALSE, showline=TRUE), xaxis=list(showline=TRUE, showgrid = FALSE))

rain <- plot_ly() %>%
  add_trace(data=climateData, x = ~Date, y = ~GF_Kisumu_rain, type = 'scatter', mode = 'lines', color=I('blue'))%>%
  layout(yaxis = list(side = 'left', title = 'Total rainfall (mm)', showgrid = FALSE, showline=TRUE), xaxis=list(showline=TRUE, showgrid = FALSE))

humidity <- plot_ly() %>%
  add_trace(data=climateData, x = ~Date, y = ~GF_Kisumu_humidity, type = 'scatter', mode = 'lines', color=I('orange'))%>%
  layout(yaxis = list(side = 'left', title = 'Relative humidity', showgrid = FALSE, showline=TRUE), xaxis=list(showline=TRUE, showgrid = FALSE))

subplot(temp,rain,humidity, nrows = 3, shareX = T, titleX = FALSE, titleY = TRUE)%>%layout(showlegend= FALSE)

# plot climate variables across sites ------------------------------------------------
# reformat from wide to long
climateDataLong <- melt(climateData, id.vars="Date")
climateDataLong$Site <- substr(climateDataLong$variable, 4,5)
climateDataLong$Site <- mapvalues(climateDataLong$Site, from=c("Ch", "Hu", "Ki", "Ma", "Ms", "Po", "Uk", "Za"), to=c("Chulaimbo", "Huaquillas", "Kisumu", "Machala", "Masambweni", "Portovelo", "Ukunda", "Zaruma"))
climateDataLong <- subset(climateDataLong, !is.na(value))

# subset  
temp <- subset(climateDataLong, variable == "GF_Chulaimbo_mean_temp"|variable == "GF_Kisumu_mean_temp"|variable == "GF_Msambweni_mean_temp"|variable == "GF_Ukunda_mean_temp"|variable == "GF_Machala_mean_temp"|variable == "GF_Huaquillas_mean_temp"|variable == "GF_Portovelo_mean_temp"|variable == "GF_Zaruma_mean_temp")
rain <- subset(climateDataLong, variable == "GF_Chulaimbo_rain"|variable == "GF_Kisumu_rain"|variable == "GF_Msambweni_rain"|variable == "GF_Ukunda_rain"|variable == "GF_Machala_rain"|variable == "GF_Huaquillas_rain"|variable == "GF_Portovelo_rain"|variable == "GF_Zaruma_rain")
hum <- subset(climateDataLong, variable == "GF_Chulaimbo_humidity"|variable == "GF_Kisumu_humidity"|variable == "GF_Msambweni_humidity"|variable == "GF_Ukunda_humidity"|variable == "GF_Machala_humidity"|variable == "GF_Huaquillas_humidity"|variable == "GF_Portovelo_humidity"|variable == "GF_Zaruma_humidity")

# plot
ggplot(data=temp, aes(x=Date, y = value, group=Site)) + geom_line() + facet_wrap(~Site) + theme_bw() + ylab("Temperature (C)")
ggplot(data=rain, aes(x=Date, y = value, group=Site)) + geom_line() + facet_wrap(~Site) + theme_bw() + ylab("Rainfall (mm)")
ggplot(data=hum, aes(x=Date, y = value, group=Site)) + geom_line() + facet_wrap(~Site) + theme_bw() + ylab("Relative humidity (%)")

# mean and range across sites
ggplot(temp, aes(x=Site, y=value, fill=Site)) + geom_boxplot() + theme_bw()
ggplot(rain, aes(x=Site, y=value, fill=Site)) + geom_boxplot() + theme_bw()
ggplot(hum, aes(x=Site, y=value, fill=Site)) + geom_boxplot() + theme_bw()
