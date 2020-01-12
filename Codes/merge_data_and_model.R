# merge observed data and modeled data ---------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(tidyverse)

# load case data
load("Concatenated_Data/case_data/merged_case_data.RData")

# load vector data
load("Concatenated_Data/vector_data/merged_vector_data.RData")

# load and format modeled data
models <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_THR_test_diff_rain_functions.csv", head=T, stringsAsFactors = F)
models$Mtot <- models$M1 + models$M2 + models$M3
models$Date <- as.Date(models$Date, "%Y-%m-%d")

# load best model info
dengue_mods <- read.csv("Concatenated_Data/model_assessment/correlation_best_mod_dengue.csv", head=T, stringsAsFactors=F)
aedes_mods <- read.csv("Concatenated_Data/model_assessment/correlation_best_mod_aedes.csv", head=T, stringsAsFactors=F)

# merge case data and modeled cases
cases_and_mods <- dengue_mods[,c("Site", "Rain_function")] %>% 
  left_join(models[,c("Site", "Date", "time", "I", "Rain_function")], by=c("Site", "Rain_function")) %>%
  left_join(cases, by=c("Site", "Date"))

save(cases_and_mods, file="Concatenated_Data/model_v_data_cases.RData")

# merge vector data and modeled cases
vectors_and_mods <- aedes_mods[,c("Site", "Rain_function")] %>% 
  left_join(models[,c("Site", "Date", "time", "Mtot", "Rain_function")], by=c("Site", "Rain_function")) %>%
  left_join(vectors, by=c("Site", "Date"))

save(vectors_and_mods, file="Concatenated_Data/model_v_data_aedes.RData")
