# merge climate data -----------------------------------------
rm(list=ls()) #remove previous variable assignments

# load data
source("Codes/concat_and_gapfill_climate_data_from_Kenya.R")
source("Codes/concat_climate_data_from_Ecuador.R")

climateDataEcuador <- read.csv("Concatenated_Data/climate_data/gapfilled_climate_data_Ecuador.csv", head=T)
climateDataKenya <- read.csv("Concatenated_Data/climate_data/gapfilled_climate_data_Kenya.csv", head=T)

# subset and merge data
climateDataEcuador <- climateDataEcuador[complete.cases(climateDataEcuador),]
climateData <- merge(climateDataKenya, climateDataEcuador, by="Date", all=T)
climateData$Date <- as.Date(climateData$Date, "%Y-%m-%d")

# save data
write.csv(climateData, "Concatenated_Data/climate_data/merged_climate_data.csv", row.names = F)
