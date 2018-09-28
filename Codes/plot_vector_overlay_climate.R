#####################################################################
#################### climate data, saved 4/21/2017 ###################
#####################################################################
library(lubridate)
library(plotly)
library(plyr)
rm(list=ls()) #remove previous variable assignments

coastClimate <- read.csv("Climate/coastClimate/stanford_rainfall_2016.csv", head=T)
coastClimate$Date <- ymd(as.character(coastClimate$Date))

plot_ly(coastClimate, x = ~Date, y = ~CHC, type = 'scatter', mode = 'lines', name = 'chc')%>%
  add_trace(y = ~OCH, name = 'och', mode = 'lines+markers')%>%
  add_trace(y = ~CHCLTM, name = 'CHCLTM', mode = 'lines+markers')%>%
  add_trace(y = ~MDH, name = 'MDH', mode = 'lines+markers')

climateMeasured <- read.csv("Climate/coastClimate/Daily_climate_data_temp_dewpoint_and_humidity_for_Msambweni_Feb_2017.csv", head=T)
climateMeasured$Date <- as.Date(climateMeasured$Date, "%d-%b-%y")

# may need to read in vector data
vector <- read.csv("monthlyVectorDataConcat.csv", head=T)
vector$Date <- as.Date(vector$DateNum)
climate2 <- merge(climateMeasured, vector, by="Date", all=T)
inc <- read.csv("inc_denv_w_PCR_ 7_Apr_2017.csv", head=T)
inc2 <- inc[,c("inc_denv", "date", "date2")]
inc3 <- subset(inc2, (!is.na(inc2[,2])) & (!is.na(inc2[,3]))) 
inc3$date<-NULL
colnames(inc3)[2] <- "Date"
inc3$Date <- as.Date(inc3$Date, "%d-%b-%y")
climate3 <- merge(climate2, inc3, by="Date", all=T)
prev2 <- denV[,c("Date", "stanforddenvigg_")]
prev3 <- subset(prev2, prev2$stanforddenvigg_ == 1)

prev3 <- ddply(prev2, .(Date),
               summarise,
               numPart = length(stanforddenvigg_),
               numSick = sum(stanforddenvigg_ == 1),
               notSick = sum(stanforddenvigg_ == 0),
               prev = (sum(stanforddenvigg_ == 1)/length(stanforddenvigg_))*100)

climate4 <- merge(climate3, prev3, by="Date", all=T)

plot_ly(climate3, x = ~Date, y = ~Temp_mean, type = 'scatter', mode = 'lines', name = 'Mean temp')%>%
  add_trace(y = ~Temp_max, name = 'Max temp', mode = 'lines+markers', fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'))%>%
  add_trace(y = ~Temp_min, name = 'Min temp', mode = 'lines+markers', fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'))%>%
  add_trace(y = ~pupaeTotal, type = 'bar', name = 'Pupae total')%>%
  add_trace(y = ~inc_denv, type='scatter', name="DenV incidence")%>%
  add_trace(y = ~RH_mean, name = 'Mean RH', mode = 'lines')%>%
  add_trace(y = ~RH_max, name = 'Max RH', mode = 'lines+markers', fill = 'tonexty', fillcolor='rgba(100,0,80,0.2)', line = list(color = 'transparent'))%>%
  add_trace(y = ~Temp_min, name = 'Min RH', mode = 'lines+markers', fill = 'tonexty', fillcolor='rgba(100,0,80,0.2)', line = list(color = 'transparent'))

plot_ly(climate4, x = ~Date, y = ~Temp_mean, type = 'scatter', mode = 'lines', name = 'Mean temp')%>%
  add_trace(y = ~Temp_max, name = 'Max temp', mode = 'lines+markers', fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'))%>%
  add_trace(y = ~Temp_min, name = 'Min temp', mode = 'lines+markers', fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'))%>%
  # add_trace(y = ~RH_mean, name = 'Mean RH', mode = 'lines')%>%
  # add_trace(y = ~RH_max, name = 'Max RH', mode = 'lines+markers', fill = 'tonexty', fillcolor='rgba(100,0,80,0.2)', line = list(color = 'transparent'))%>%
  # add_trace(y = ~Temp_min, name = 'Min RH', mode = 'lines+markers', fill = 'tonexty', fillcolor='rgba(100,0,80,0.2)', line = list(color = 'transparent'))%>%
  add_trace(y = ~oviTotal, type = 'bar', name = 'ovitrap', yaxis = "y2")%>%
  add_trace(y = ~prokopakTotal, type = 'bar', name = 'prokopak', yaxis = "y2") %>%
  add_trace(y = ~hlcTotal, type = 'bar', name = 'hlc', yaxis = "y2") %>%
  add_trace(y = ~trapTotal, type = 'bar', name = 'trap', yaxis = "y2") %>%
  add_trace(y = ~pupaeTotal, type = 'bar', name = 'pupae', yaxis = "y2") %>%
  add_trace(y = ~larvalTotal, type = 'bar', name = 'larvae', yaxis = "y2") %>%
  layout(yaxis2 = list(overlaying = "y", side='right'), barmode='relative') %>%
  add_trace(y = ~prev, type = 'scatter', mode = 'lines')

plot_ly(climate4, x = ~Date, y = ~prev, type='scatter', mode = 'lines')%>%
  add_trace(y = ~Temp_mean, type = 'scatter', mode = 'lines', name = 'Mean temp')

hcc <- read.csv("hcc_elisa.csv", head=T)

rainMS <- read.csv("Climate/coastClimate/Coast_daily_rainfall_Mar_2017_ms.csv", head=T)
rainMS$Date <- as.Date(rainMS$Date, "%m/%d/%Y")
rainSatCoast <- read.csv("Climate/coastClimate/stanford_rainfall_2016.csv", head=T)
rainSatCoast$Date <- ymd(rainSatCoast$Date)
rainMScon <- merge(rainMS, rainSatCoast, by="Date")

rainMScon$Rain_mm[rainMScon$Rain_mm == 78994] <- 79

plot_ly(rainMScon, x=~Date, y=~Rain_mm, type='scatter', mode='lines', name='observed')%>%
  add_trace(y=~MDH, mode='lines', name='MDH')%>%
  add_trace(y=~MDHLTM, mode='lines', name='MDHLTM')%>%
  layout(title='Msambweni rainfall (mm)')

rainUK <- read.csv("Climate/coastClimate/Coast_daily_rainfall_Mar_2017_uk.csv", head=T)
rainUK$Date <- as.Date(rainUK$Date, "%m/%d/%Y")

# diani health centre = ukunda hospital
rainUKcon <- merge(rainUK, rainSatCoast, by="Date")

plot_ly(rainUKcon, x=~Date, y=~Rain_mm, type='scatter', mode='lines', name='observed')%>%
  add_trace(y=~DHC, mode='lines', name='DHC')%>%
  add_trace(y=~DHCLTM, mode='lines', name='DHCLTM')%>%
  layout(title='Ukunda rainfall (mm)')

rainWest <- read.csv("Climate/westClimate/Rainfall_Daily_Data_Oct_3_2016.csv", head=T)
rainWest$Date <- as.Date(rainWest$Date, "%d-%b-%y")
rainWestCon <- merge(rainWest, rainSatCoast, by = "Date")
