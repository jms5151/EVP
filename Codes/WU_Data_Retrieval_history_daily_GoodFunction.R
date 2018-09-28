rm(list=ls()) #remove previous variable assignments
library(plyr)
library(rwunderground)

source("C:/Users/Jamie/Box Sync/R_functions/Weather_Underground_Key.R")

west.date.range <- seq.Date(from=as.Date('2014-03-28'), to=as.Date('2017-09-04'), by='1 day')
coast.date.range <- seq.Date(from=as.Date('2013-11-23'), to=as.Date('2017-8-16'), by='1 day')

#--- west data retrival
west <- data.frame() # Initialize a data frame

# loop over dates, and fetch weather data
for(i in 1:996) {
  Sys.sleep(7)
  dataForDate <- history_daily("HKKI", date = west.date.range[i], use_metric = T, key = wu.key)
  west <- rbind(west, dataForDate)
}

write.csv(west, "Climate/westClimate/wu_west.csv", row.names=FALSE)

#--- coast data retrival
coast <- data.frame() # Initialize a data frame OR

# loop over dates, and fetch weather data
for(i in 1:496) {
  Sys.sleep(7)
  dataForDate <- history_daily("HKMO", date = coast.date.range[i], use_metric = T, key = wu.key)
  coast <- rbind(coast, dataForDate)
}

write.csv(coast, "Climate/coastClimate/wu_coast_500Entries.csv", row.names=FALSE)
