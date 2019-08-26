# merge climate data -----------------------------------------
rm(list=ls()) #remove previous variable assignments

# load data
# source("Codes/concat_and_gapfill_climate_data_from_Kenya.R")
# source("Codes/concat_and_gapfill_climate_data_from_Ecuador.R")
climateDataEcuador <- read.csv("Concatenated_Data/climate_data/gapfilled_climate_data_Ecuador.csv", head=T, stringsAsFactors = F)
climateDataKenya <- read.csv("Concatenated_Data/climate_data/gapfilled_climate_data_Kenya.csv", head=T, stringsAsFactors = F)

# merge data
climateData <- rbind(climateDataKenya, climateDataEcuador)

# sort data
climateData$Date <- as.Date(climateData$Date, "%Y-%m-%d")
climateData <- climateData[order(climateData$Date, climateData$Site),]

# add county 
climateData$country <- ifelse(climateData$Site=="Chulaimbo"|climateData$Site=="Kisumu"|climateData$Site=="Msambweni"|climateData$Site=="Ukunda", "Kenya", "Ecuador")

# adjust RH if above 100
climateData$Humidity <- ifelse(climateData$Humidity > 100, 100, climateData$Humidity)

# calculate saturation vapor pressure deficit
# http://cronklab.wikidot.com/calculation-of-vapour-pressure-deficit
climateData$SVP <- 610.7*107.5*climateData$Temperature/(237.3+climateData$Temperature)
climateData$SVPD = (((100 - climateData$Humidity)/100)*climateData$SVP)/1000
climateData$SVP <- NULL

# save data
save(climateData, file="Concatenated_Data/climate_data/merged_climate_data.RData")
