# merge observed data and modeled data ---------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(plyr)

# load case data
load("Concatenated_Data/case_data/merged_case_data.RData")

# load vector data
load("Concatenated_Data/vector_data/merged_vector_data.RData")

# combine case and vector data
obs.data <- merge(cases, vectors, by=c("Site", "Date"), all=T)

# load and format modeled data
models <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_TRH_final_model.csv", head=T, stringsAsFactors = F)
models$Mtot <- models$M1 + models$M2 + models$M3

# format date
models$Date <- as.Date(models$Date, "%Y-%m-%d")

# combine case data, vector data, and modeled data
merged_data <- merge(models[,c("Site", "Date", "I", "Mtot")], obs.data, by=c("Site", "Date"), all=T)

# add country
merged_data$Country <- ifelse(merged_data$Site=="Chulaimbo"|merged_data$Site=="Kisumu"|merged_data$Site=="Msambweni"|merged_data$Site=="Ukunda", "Kenya", "Ecuador")

# save data
save(merged_data, file="Concatenated_Data/model_v_data.RData")
