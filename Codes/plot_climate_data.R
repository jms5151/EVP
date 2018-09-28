# climate plots --------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(plotly)

# load data
climateData <- read.csv("Kenya/Concatenated_Data/climate/gapfilled_climate_data.csv", head=T)
climateData$Date <- as.Date(climateData$Date, "%Y-%m-%d")

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


# overlaid plots - very messy ------------------------
# plot_ly() %>%
#   add_trace(data=climateData, x = ~Date, y = ~GF_Kisumu_mean_temp, type = 'scatter', mode='lines', color=I('darkred'))%>%
#   add_trace(data=climateData, x = ~Date, y = ~GF_Kisumu_rain, type = 'bar', color=I('blue'), opacity=0.7, yaxis = 'y2')%>%
#   add_trace(data=climateData, x = ~Date, y = ~GF_Kisumu_humidity, type = 'scatter', mode='lines', color=I('orange'), opacity=0.7, yaxis = 'y3')%>%
#   layout(margin=list(l=100,r=60,b=100,t=50),
#          yaxis = list(side = 'left', title = 'Temperature (degrees C)', showgrid = FALSE, showline=TRUE), 
#          xaxis=list(showline=FALSE, showgrid = FALSE),
#          yaxis2 = list(overlaying = "y", side='left', title = 'Total rain (mm)'), showlegend = FALSE,
#          yaxis3 = list(overlaying = "y", side='left', title = 'Relative humidity (%)'), showlegend = FALSE)
