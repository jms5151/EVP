# calculate RMSE scores ------------------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(plyr)
library(Metrics)
library(reshape2)

# https://www.marinedatascience.co/blog/2019/01/07/normalizing-the-rmse/
# https://github.com/saskiaotto/INDperform

# functions
zscore <- function(x){(x - mean(x,na.rm=T)) / sd(x,na.rm=T)}

nrmse_func <-  function(obs, pred, type = "sd") {
  
  squared_sums <- sum((obs - pred)^2)
  mse <- squared_sums/length(obs)
  rmse <- sqrt(mse)
  if (type == "sd") nrmse <- rmse/sd(obs)
  if (type == "mean") nrmse <- rmse/mean(obs)
  if (type == "maxmin") nrmse <- rmse/ (max(obs) - min(obs))
  if (type == "iq") nrmse <- rmse/ (quantile(obs, 0.75) - quantile(obs, 0.25))
  if (!type %in% c("mean", "sd", "maxmin", "iq")) message("Wrong type!")
  nrmse <- round(nrmse, 3)
  return(nrmse)
  
}

# load case data
load("Concatenated_Data/case_data/merged_case_data.RData")

# load vector data
load("Concatenated_Data/vector_data/merged_vector_data.RData")

# combine case and vector data
obs.data <- merge(cases[,c("Site", "Date", "denv_positive")], vectors[,c("Site", "Date", "aedes_total")], by=c("Site", "Date"), all=T)

# RMSE calculations for temperature simulations ------------------------------
# load and format modeled data for temperature simulations
models <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_temperature.csv", head=T, stringsAsFactors = F)
models$Mtot_temperature <- models$M1 + models$M2 + models$M3
models$I_temperature <- models$I
models$Date <- as.Date(models$Date, "%Y-%m-%d")

# combine case data, vector data, and modeled data for temperature simulations
merged_data_temp <- merge(models[,c("Site", "Date", "I_temperature", "Mtot_temperature")], obs.data, by=c("Site", "Date"), all=T)

# calculate RMSE
siteNames <- unique(merged_data_temp$Site)
obs.vars <- c("aedes_total", "denv_positive")
pred.vars <- c("Mtot_temperature", "I_temperature")
rmse_temp <- data.frame(matrix(ncol=4,nrow=0))
colnames(rmse_temp) <- c("Site", "Yvar", "RMSE_temp", "n")

for (siteX in siteNames){
  df <- subset(merged_data_temp, Site == siteX)
  for (i in 1:length(obs.vars)){
    df2 <- df[,c(obs.vars[i], pred.vars[i])]
    df2 <- df2[complete.cases(df2),]
    N <- nrow(df2)
    df2 <- lapply(df2, zscore)
    # rmse <- rmse(df2[[1]], df2[[2]])
    # rmse <- nrmse_func(df2[[1]], df2[[2]], type='sd')
    rmse <- summary(lm(df2[[1]]~df2[[2]]))$r.squared
    tmp_vec <- c(siteX, obs.vars[i], round(rmse,2), N)
    rmse_temp[nrow(rmse_temp)+1,] <- tmp_vec
  }
}

write.csv(rmse_temp, "Concatenated_Data/model_assessment/RMSE_temperature_model_by_region.csv", row.names=F)

rmse_temp[,c("RMSE_temp", "n")] <- lapply(rmse_temp[,c("RMSE_temp", "n")], as.numeric)
temp_weighted_rmse <- sum(rmse_temp$RMSE_temp*rmse_temp$n)/sum(rmse_temp$n)

# RMSE calculations for temperature and rainfall simulations ----------------------
models <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_TR_test_diff_rain_functions.csv", head=T, stringsAsFactors = F)
models$Mtot_rainfall <- models$M1 + models$M2 + models$M3
models$I_rainfall <- models$I
models$Date <- as.Date(models$Date, "%Y-%m-%d")

# combine case data, vector data, and modeled data for temperature simulations
merged_data_rain <- merge(models[,c("Site", "Date", "I_rainfall", "Mtot_rainfall", "Rain_function")], obs.data, by=c("Site", "Date"), all=T)

# calculate RMSE
siteNames <- unique(merged_data_rain$Site)
obs.vars <- c("aedes_total", "denv_positive")
pred.vars <- c("Mtot_rainfall", "I_rainfall")
rain.fun <-  c("Right_skewed", "Quadratic", "Briere")
rmse_rain <- data.frame(matrix(ncol=5,nrow=0))
colnames(rmse_rain) <- c("Site", "Yvar", "Rain_function", "RMSE_rain", "n")

for (siteX in siteNames){
  df <- subset(merged_data_rain, Site == siteX)
  for (fun in rain.fun){
    df2 <- subset(df, Rain_function == fun)
    for (i in 1:length(obs.vars)){
      df3 <- df2[,c(obs.vars[i], pred.vars[i])]
      df3 <- df3[complete.cases(df3),]
      N <- nrow(df3)
      # df3 <- lapply(df3, zscore)
      # rmse <- rmse(df3[[1]], df3[[2]])
      # rmse <- nrmse_func(df3[[1]], df3[[2]], type='sd')
      rmse <- summary(lm(df3[[1]]~df3[[2]]))$r.squared
      tmp_vec <- c(siteX, obs.vars[i], fun, round(rmse,2), N)
      rmse_rain[nrow(rmse_rain)+1,] <- tmp_vec
    }
  }
}

# format
rmse_rain2 <- melt(rmse_rain[,c("Site", "Yvar", "Rain_function", "RMSE_rain")], idvars=c("Site", "Yvar", "n"))#
rmse_rain2 <- dcast(rmse_rain2, Site+Yvar~Rain_function)

write.csv(rmse_rain2, "Concatenated_Data/model_assessment/RMSE_diff_rain_models_by_site.csv", row.names=F)

# combine rmse outputs and format for easy interpretation
rmse_combined <- merge(rmse_temp, rmse_rain2, by=c("Site", "Yvar"))
rmse_combined$Country <- ifelse(rmse_combined$Site=="Chulaimbo"|rmse_combined$Site=="Kisumu"|rmse_combined$Site=="Msambweni"|rmse_combined$Site=="Ukunda", "Kenya", "Ecuador")
rmse_combined[,c("RMSE_temp", "n", "Briere", "Quadratic", "Right_skewed")] <- lapply(rmse_combined[,c("RMSE_temp", "n", "Briere", "Quadratic", "Right_skewed")], as.numeric)
# rmse_combined$delta_rain_briere <- rmse_combined$RMSE_temp - rmse_combined$Briere
# rmse_combined$delta_rain_quadratic <- rmse_combined$RMSE_temp - rmse_combined$Quadratic
# rmse_combined$delta_rain_right_skewed <- rmse_combined$RMSE_temp - rmse_combined$Right_skewed
# 
# rain_BR_all <- sum(rmse_combined$delta_rain_briere*rmse_combined$n)/sum(rmse_combined$n)
# rain_QD_all <- sum(rmse_combined$delta_rain_quadratic*rmse_combined$n)/sum(rmse_combined$n)
# rain_RS_all <- sum(rmse_combined$delta_rain_right_skewed*rmse_combined$n)/sum(rmse_combined$n)

rain_BR_all <- sum(rmse_combined$Briere*rmse_combined$n)/sum(rmse_combined$n)
rain_QD_all <- sum(rmse_combined$Quadratic*rmse_combined$n)/sum(rmse_combined$n)
rain_RS_all <- sum(rmse_combined$Right_skewed*rmse_combined$n)/sum(rmse_combined$n)

e <- subset(rmse_combined, Country == "Ecuador")
k <- subset(rmse_combined, Country == "Kenya")

# rain_BR_e <- sum(e$delta_rain_briere*e$n)/sum(e$n)
# rain_QD_e <- sum(e$delta_rain_quadratic*e$n)/sum(e$n)
# rain_RS_e <- sum(e$delta_rain_right_skewed*e$n)/sum(e$n)
# rain_BR_k <- sum(k$delta_rain_briere*k$n)/sum(k$n)
# rain_QD_k <- sum(k$delta_rain_quadratic*k$n)/sum(k$n)
# rain_RS_k <- sum(k$delta_rain_right_skewed*k$n)/sum(k$n)

rain_BR_e <- sum(e$Briere*e$n)/sum(e$n)
rain_QD_e <- sum(e$Quadratic*e$n)/sum(e$n)
rain_RS_e <- sum(e$Right_skewed*e$n)/sum(e$n)
rain_BR_k <- sum(k$Briere*k$n)/sum(k$n)
rain_QD_k <- sum(k$Quadratic*k$n)/sum(k$n)
rain_RS_k <- sum(k$Right_skewed*k$n)/sum(k$n)

rmse_combined$r2 <- ifelse(rmse_combined$Country == "Kenya", rmse_combined$Briere, rmse_combined$Quadratic)

rain_all <- sum(rmse_combined$r2*rmse_combined$n)/sum(as.numeric(rmse_combined$n))

# RMSE calculations for temperature, rainfall, and humidity simulations ----------------------
models <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_TRH_test_diff_hum_functions.csv", head=T, stringsAsFactors = F)
models$Mtot_humidity <- models$M1 + models$M2 + models$M3
models$I_humidity <- models$I
models$Date <- as.Date(models$Date, "%Y-%m-%d")

# combine case data, vector data, and modeled data for temperature simulations
merged_data_humidity <- merge(models[,c("Site", "Date", "I_humidity", "Mtot_humidity", "Humidity_function")], obs.data, by=c("Site", "Date"), all=T)

# calculate RMSE
siteNames <- unique(merged_data_humidity$Site)
obs.vars <- c("aedes_total", "denv_positive")
pred.vars <- c("Mtot_humidity", "I_humidity")
hum.fun <-  c("linear", "sigmoidal")
rmse_humidity <- data.frame(matrix(ncol=5,nrow=0))
colnames(rmse_humidity) <- c("Site", "Yvar", "Humidity_function", "RMSE_humidity", "n")

for (siteX in siteNames){
  df <- subset(merged_data_humidity, Site == siteX)
  for (fun in hum.fun){
    df2 <- subset(df, Humidity_function == fun)
    for (i in 1:length(obs.vars)){
      df3 <- df2[,c(obs.vars[i], pred.vars[i])]
      df3 <- df3[complete.cases(df3),]
      N <- nrow(df3)
      if (siteX == "Zaruma" & obs.vars[i] == "denv_positive"){
        df3 <- df3
      } else{
        df3 <- lapply(df3, zscore)
      }
      # rmse <- nrmse_func(df3[[1]], df3[[2]], typd='sd')
      rmse <- summary(lm(df3[[1]]~df3[[2]]))$r.squared
      tmp_vec <- c(siteX, obs.vars[i], fun, round(rmse,2), N)
      rmse_humidity[nrow(rmse_humidity)+1,] <- tmp_vec
    }
  }
}

# format
rmse_humidity2 <- melt(rmse_humidity[,c("Site", "Yvar", "Humidity_function", "RMSE_humidity")], idvars=c("Site", "Yvar", "n"))#
rmse_humidity2 <- dcast(rmse_humidity2, Site+Yvar~Humidity_function)

# combine rmse outputs and format for easy interpretation
rmse_combined <- merge(rmse_combined, rmse_humidity2, by=c("Site", "Yvar"))
rmse_combined$Country <- ifelse(rmse_combined$Site=="Chulaimbo"|rmse_combined$Site=="Kisumu"|rmse_combined$Site=="Msambweni"|rmse_combined$Site=="Ukunda", "Kenya", "Ecuador")
rmse_combined <- rmse_combined[order(rmse_combined$Country, rmse_combined$Site), c("Country", "Site", "Yvar", "RMSE_temp", "Briere", "Quadratic", "Right_skewed", "delta_rain_briere", "delta_rain_quadratic", "delta_rain_right_skewed", "linear", "sigmoidal", "n")]

write.csv(rmse_combined, "Concatenated_Data/model_assessment/RMSE_TRH_models_by_site.csv", row.names=F)

# calculate weighted means
rmse_combined$RMSE_temp_rain <- ifelse(rmse_combined$Country=="Ecuador", rmse_combined$Right_skewed, rmse_combined$Briere)

rmse_combined[,c("linear", "sigmoidal")] <- lapply(rmse_combined[,c("linear", "sigmoidal")], as.numeric)
rmse_combined$delta_humidity_linear <- rmse_combined$RMSE_temp_rain - rmse_combined$linear
rmse_combined$delta_humidity_sigmoidal <- rmse_combined$RMSE_temp_rain - rmse_combined$sigmoidal

write.csv(rmse_combined, "Concatenated_Data/model_assessment/RMSE_TRH_models_by_site_with_delta_vals.csv", row.names=F)

# weightedMeans <- data.frame(matrix(ncol=))
# hum_lin <- sum(rmse_combined$delta_humidity_linear*rmse_combined$n)/sum(rmse_combined$n)
# hum_sig <- sum(rmse_combined$delta_humidity_sigmoidal*rmse_combined$n)/sum(rmse_combined$n)

hum_lin <- sum(rmse_combined$linear*rmse_combined$n)/sum(rmse_combined$n)
hum_sig <- sum(rmse_combined$sigmoidal*rmse_combined$n)/sum(rmse_combined$n)

hum_sig_mosq <- sum(rmse_combined$sigmoidal[rmse_combined$Yvar=="aedes_total"]*rmse_combined$n[rmse_combined$Yvar=="aedes_total"])/sum(rmse_combined$n[rmse_combined$Yvar=="aedes_total"])
hum_sig_denv <- sum(rmse_combined$sigmoidal[rmse_combined$Yvar=="denv_positive"]*rmse_combined$n[rmse_combined$Yvar=="denv_positive"])/sum(rmse_combined$n[rmse_combined$Yvar=="denv_positive"])


weightedMeans <- c(rain_BR_all, rain_QD_all, rain_RS_all, rain_BR_e, rain_QD_e, rain_RS_e, rain_BR_k, rain_QD_k, rain_RS_k, hum_lin, hum_sig)
weightedMeans <- round(weightedMeans, 3)
names(weightedMeans) <- c("rain_BR_all", "rain_QD_all", "rain_RS_all", "rain_BR_e", "rain_QD_e", "rain_RS_e", "rain_BR_k", "rain_QD_k", "rain_RS_k", "hum_lin", "hum_sig")
weightedMeans <- as.data.frame(weightedMeans)

write.csv(weightedMeans, "Concatenated_Data/model_assessment/RMSE_TRH_models_weighted_means.csv", row.names=T)

# calculate RMSE values for other mosquito life stages
models <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_TRH_final_model.csv", head=T, stringsAsFactors = F)
models$Mtot <- models$M1 + models$M2 + models$M3
models$I <- models$I
models$Date <- as.Date(models$Date, "%Y-%m-%d")
obs.data2 <- merge(cases, vectors, by=c("Site", "Date"), all=T)
merged_data <- merge(models[,c("Site", "Date", "I", "Mtot")], obs.data2, by=c("Site", "Date"), all=T)

siteNames <- c("Chulaimbo", "Kisumu", "Msambweni", "Ukunda")
obs.vars <- c("pupae_total", "early_instar_total", "late_instar_total", "egg_total_adjusted")
rmse.final <- data.frame(matrix(ncol=4,nrow=0))
colnames(rmse.final) <- c("Site", "Yvar", "RMSE", "n")

for (siteX in siteNames){
  df <- subset(merged_data, Site == siteX)
  for (i in 1:length(obs.vars)){
    df2 <- df[,c(obs.vars[i], "Mtot")]
    df2 <- df2[complete.cases(df2),]
    N <- nrow(df2)
    df2 <- lapply(df2, zscore)
    # rmse <- rmse(df2[[1]], df2[[2]])
    rmse <- summary(lm(df2[[1]]~df2[[2]]))$r.squared
    tmp_vec <- c(siteX, obs.vars[i], round(rmse,2), N)
    rmse.final[nrow(rmse.final)+1,] <- tmp_vec
  }
}

# format
rmse.final$RMSE <- as.numeric(rmse.final$RMSE)
rmse_final2 <- melt(rmse.final[,c("Site", "Yvar", "RMSE")], idvars=c("Site", "Yvar"))#
rmse_final2 <- dcast(rmse_final2, Site~Yvar)
write.csv(rmse_final2, "Concatenated_Data/model_assessment/RMSE_TRH_models_other_mosquito_life_stages.csv", row.names=F)

rmse_final3 <- melt(rmse.final[,c("Site", "Yvar", "n")], idvars=c("Site", "Yvar"))#
rmse_final3 <- dcast(rmse_final3, Site~Yvar)
write.csv(rmse_final3, "Concatenated_Data/model_assessment/RMSE_TRH_models_other_mosquito_life_stages_sample_sizes.csv", row.names=F)

adults <- sum(rmse_combined$sigmoidal[rmse_combined$Yvar=="aedes_total"&rmse_combined$Country=="Kenya"]*rmse_combined$n[rmse_combined$Yvar=="aedes_total"&rmse_combined$Country=="Kenya"])/sum(rmse_combined$n[rmse_combined$Yvar=="aedes_total"&rmse_combined$Country=="Kenya"])
eggs <- sum(rmse.final$RMSE[rmse.final$Yvar=="egg_total_adjusted"]*as.numeric(rmse.final$n[rmse.final$Yvar=="egg_total_adjusted"]))/sum(as.numeric(rmse.final$n[rmse.final$Yvar=="egg_total_adjusted"]))
early_instar <- sum(rmse.final$RMSE[rmse.final$Yvar=="early_instar_total"]*as.numeric(rmse.final$n[rmse.final$Yvar=="early_instar_total"]))/sum(as.numeric(rmse.final$n[rmse.final$Yvar=="early_instar_total"]))
late_instar <- sum(rmse.final$RMSE[rmse.final$Yvar=="late_instar_total"]*as.numeric(rmse.final$n[rmse.final$Yvar=="late_instar_total"]))/sum(as.numeric(rmse.final$n[rmse.final$Yvar=="late_instar_total"]))
pupae <- sum(rmse.final$RMSE[rmse.final$Yvar=="pupae_total"]*as.numeric(rmse.final$n[rmse.final$Yvar=="pupae_total"]))/sum(as.numeric(rmse.final$n[rmse.final$Yvar=="pupae_total"]))

eggs <- summary(lm(egg_total_adjusted~Mtot+Site, merged_data))$r.squared
