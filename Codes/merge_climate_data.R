# merge climate data -----------------------------------------
rm(list=ls()) #remove previous variable assignments

# load data
# source("Codes/concat_and_gapfill_climate_data_from_Kenya.R")
# source("Codes/concat_and_gapfill_climate_data_from_Ecuador.R")
climateDataEcuador <- read.csv("Concatenated_Data/climate_data/gapfilled_climate_data_Ecuador_2000-2018.csv", head=T)
climateDataKenya <- read.csv("Concatenated_Data/climate_data/gapfilled_climate_data_Kenya.csv", head=T)

# merge data
climateData <- merge(climateDataKenya, climateDataEcuador, by="Date", all=T)

# sort data
climateData$Date <- as.Date(climateData$Date, "%Y-%m-%d")
climateData <- climateData[order(climateData$Date),]

# save data
write.csv(climateData, "Concatenated_Data/climate_data/merged_climate_data.csv", row.names = F)
