# calculate RMSE scores ------------------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
# library(plyr)
# library(Metrics)
# library(reshape2)

# load case data
load("Concatenated_Data/case_data/merged_case_data.RData")

# load vector data
load("Concatenated_Data/vector_data/merged_vector_data.RData")

# combine case and vector data
obs.data <- merge(cases[,c("Site", "Date", "denv_positive")], vectors[,c("Site", "Date", "aedes_total")], by=c("Site", "Date"), all=T)
obs.data$Country <- ifelse(obs.data$Site=="Chulaimbo"|obs.data$Site=="Kisumu"|obs.data$Site=="Msambweni"|obs.data$Site=="Ukunda", "Kenya", "Ecuador")

n_mosq <- sum(!is.na(obs.data$aedes_total)==TRUE)
n_denv <- sum(!is.na(obs.data$denv_positive)==TRUE)
n_mosq_e <- sum(!is.na(obs.data$aedes_total[obs.data$Country=="Ecuador"])==TRUE)
n_denv_e <- sum(!is.na(obs.data$denv_positive[obs.data$Country=="Ecuador"])==TRUE)
n_mosq_k <- sum(!is.na(obs.data$aedes_total[obs.data$Country=="Kenya"])==TRUE)
n_denv_k <- sum(!is.na(obs.data$denv_positive[obs.data$Country=="Kenya"])==TRUE)

# R2 calculations for temperature simulations ------------------------------
# load and format modeled data for temperature simulations
models <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_temperature.csv", head=T, stringsAsFactors = F)
models$Mtot_temperature <- models$M1 + models$M2 + models$M3
models$I_temperature <- models$I
models$Date <- as.Date(models$Date, "%Y-%m-%d")

# combine case data, vector data, and modeled data for temperature simulations
merged_data_temp <- merge(models[,c("Site", "Date", "I_temperature", "Mtot_temperature")], obs.data, by=c("Site", "Date"), all=T)

# calculate temperature model adjusted R2
R2_temp_mosq <- summary(lm(aedes_total~Mtot_temperature+Site, merged_data_temp))$r.squared
R2_temp_denv <- summary(lm(denv_positive~I_temperature+Site, merged_data_temp))$r.squared
R2_temp_mean <- (R2_temp_mosq*n_mosq+R2_temp_denv*n_denv)/(n_mosq+n_denv)

# Adjusted R2 calculations for temperature and rainfall simulations ------------------
models <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_TR_test_diff_rain_functions.csv", head=T, stringsAsFactors = F)
models$Mtot_rainfall <- models$M1 + models$M2 + models$M3
models$I_rainfall <- models$I
models$Date <- as.Date(models$Date, "%Y-%m-%d")

# combine case data, vector data, and modeled data for temperature simulations
merged_data_rain <- merge(models[,c("Site", "Date", "I_rainfall", "Mtot_rainfall", "Rain_function")], obs.data, by=c("Site", "Date"), all=T)

rain_BR_all_denv <- summary(lm(denv_positive~I_rainfall+Site, merged_data_rain[merged_data_rain$Rain_function=="Briere",]))$r.squared
rain_BR_all_mosq <- summary(lm(aedes_total~Mtot_rainfall+Site, merged_data_rain[merged_data_rain$Rain_function=="Briere",]))$r.squared
rain_BR_all_mean <- (rain_BR_all_denv*n_denv+rain_BR_all_mosq*n_mosq)/(n_denv+n_mosq)

rain_QD_all_denv <- summary(lm(denv_positive~I_rainfall+Site, merged_data_rain[merged_data_rain$Rain_function=="Quadratic",]))$r.squared
rain_QD_all_mosq <- summary(lm(aedes_total~Mtot_rainfall+Site, merged_data_rain[merged_data_rain$Rain_function=="Quadratic",]))$r.squared
rain_QD_all_mean <- (rain_QD_all_denv*n_denv+rain_QD_all_mosq*n_mosq)/(n_denv+n_mosq)

rain_RS_all_denv <- summary(lm(denv_positive~I_rainfall+Site, merged_data_rain[merged_data_rain$Rain_function=="Right_skewed",]))$r.squared
rain_RS_all_mosq <- summary(lm(aedes_total~Mtot_rainfall+Site, merged_data_rain[merged_data_rain$Rain_function=="Right_skewed",]))$r.squared
rain_RS_all_mean <- (rain_RS_all_denv*n_denv+rain_RS_all_mosq*n_mosq)/(n_denv+n_mosq)

# Adjusted R2 for Ecuador  
e <- subset(merged_data_rain, Country == "Ecuador")

rain_BR_equador_denv <- summary(lm(denv_positive~I_rainfall+Site, e[e$Rain_function=="Briere",]))$r.squared
rain_BR_equador_mosq <- summary(lm(aedes_total~Mtot_rainfall+Site, e[e$Rain_function=="Briere",]))$r.squared
rain_BR_equador_mean <- (rain_BR_equador_denv*n_denv_e+rain_BR_equador_mosq*n_mosq_e)/(n_denv_e+n_mosq_e)

rain_QD_equador_denv <- summary(lm(denv_positive~I_rainfall+Site, e[e$Rain_function=="Quadratic",]))$r.squared
rain_QD_equador_mosq <- summary(lm(aedes_total~Mtot_rainfall+Site, e[e$Rain_function=="Quadratic",]))$r.squared
rain_QD_equador_mean <- (rain_QD_equador_denv*n_denv_e+rain_QD_equador_mosq*n_mosq_e)/(n_denv_e+n_mosq_e)

rain_RS_equador_denv <- summary(lm(denv_positive~I_rainfall+Site, e[e$Rain_function=="Right_skewed",]))$r.squared
rain_RS_equador_mosq <- summary(lm(aedes_total~Mtot_rainfall+Site, e[e$Rain_function=="Right_skewed",]))$r.squared
rain_RS_equador_mean <- (rain_RS_equador_denv*n_denv_e+rain_RS_equador_mosq*n_mosq_e)/(n_denv_e+n_mosq_e)

# R2 for Kenya
k <- subset(merged_data_rain, Country == "Kenya")

rain_BR_kenya_denv <- summary(lm(denv_positive~I_rainfall+Site, k[k$Rain_function=="Briere",]))$r.squared
rain_BR_kenya_mosq <- summary(lm(aedes_total~Mtot_rainfall+Site, k[k$Rain_function=="Briere",]))$r.squared
rain_BR_kenya_mean <- (rain_BR_kenya_denv*n_denv_k+rain_BR_kenya_mosq*n_mosq_k)/(n_denv_k+n_mosq_k)

rain_QD_kenya_denv <- summary(lm(denv_positive~I_rainfall+Site, k[k$Rain_function=="Quadratic",]))$r.squared
rain_QD_kenya_mosq <- summary(lm(aedes_total~Mtot_rainfall+Site, k[k$Rain_function=="Quadratic",]))$r.squared
rain_QD_kenya_mean <- (rain_QD_kenya_denv*n_denv_k+rain_QD_kenya_mosq*n_mosq_k)/(n_denv_k+n_mosq_k)

rain_RS_kenya_denv <- summary(lm(denv_positive~I_rainfall+Site, k[k$Rain_function=="Right_skewed",]))$r.squared
rain_RS_kenya_mosq <- summary(lm(aedes_total~Mtot_rainfall+Site, k[k$Rain_function=="Right_skewed",]))$r.squared
rain_RS_kenya_mean <- (rain_RS_kenya_denv*n_denv_k+rain_RS_kenya_mosq*n_mosq_k)/(n_denv_k+n_mosq_k)

# R2 value for model using different rainfall functions
e2 <- subset(e, Rain_function == "Quadratic")
k2 <- subset(k, Rain_function == "Briere")

rain2 <- rbind(e2, k2)
rain_final_denv <- summary(lm(denv_positive~I_rainfall+Site, rain2))$r.squared
rain_final_mosq <- summary(lm(aedes_total~Mtot_rainfall+Site, rain2))$r.squared

# Adjusted R2 calculations for temperature, rainfall, and humidity simulations ----------------------
models <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_TRH_test_diff_hum_functions.csv", head=T, stringsAsFactors = F)
models$Mtot_humidity <- models$M1 + models$M2 + models$M3
models$I_humidity <- models$I
models$Date <- as.Date(models$Date, "%Y-%m-%d")

# combine case data, vector data, and modeled data for temperature simulations
merged_data_humidity <- merge(models[,c("Site", "Date", "I_humidity", "Mtot_humidity", "Humidity_function")], obs.data, by=c("Site", "Date"), all=T)

hum_lin_all_denv <- summary(lm(denv_positive~I_humidity+Site, merged_data_humidity[merged_data_humidity$Humidity_function=="linear",]))$r.squared
hum_lin_all_mosq <- summary(lm(aedes_total~Mtot_humidity+Site, merged_data_humidity[merged_data_humidity$Humidity_function=="linear",]))$r.squared
hum_lin_all_mean <- (hum_lin_all_denv*n_denv+hum_lin_all_mosq*n_mosq)/(n_denv+n_mosq)

hum_sig_all_denv <- summary(lm(denv_positive~I_humidity+Site, merged_data_humidity[merged_data_humidity$Humidity_function=="sigmoidal",]))$r.squared
hum_sig_all_mosq <- summary(lm(aedes_total~Mtot_humidity+Site, merged_data_humidity[merged_data_humidity$Humidity_function=="sigmoidal",]))$r.squared
hum_sig_all_mean <- (hum_sig_all_denv*n_denv+hum_sig_all_mosq*n_mosq)/(n_denv+n_mosq)
