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
# R2_temp_mean <- (R2_temp_mosq*n_mosq+R2_temp_denv*n_denv)/(n_mosq+n_denv)

# summary with different intercepts
R2_temp_mosq <- summary(lm(aedes_total~Mtot_temperature, merged_data_temp))$r.squared
R2_temp_denv <- summary(lm(denv_positive~I_temperature, merged_data_temp))$r.squared

R2_temp_mosq <- summary(lm(aedes_total~Mtot_temperature+Country, merged_data_temp))$r.squared
R2_temp_denv <- summary(lm(denv_positive~I_temperature+Country, merged_data_temp))$r.squared


# Adjusted R2 calculations for temperature and rainfall simulations ------------------
models <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_TR_test_diff_rain_functions.csv", head=T, stringsAsFactors=F)
models$Mtot_rainfall <- models$M1 + models$M2 + models$M3
models$I_rainfall <- models$I
models$Date <- as.Date(models$Date, "%Y-%m-%d")
models <- merge(models, obs.data, by=c("Site", "Date"))

rFuns <- unique(models$Rain_function)
rFunsDFall <- data.frame(matrix(nrow=0, ncol=4))
colnames(rFunsDFall) <- c("Rain_function", "R2_mosq", "R2_denv", "R2_weighted_mean")

for (rainFun in rFuns){
  rSub <- subset(models, Rain_function == rainFun)
  mosq <- summary(lm(aedes_total~Mtot_rainfall+Site, models[models$Rain_function==rainFun,]))$r.squared
  denv <- summary(lm(denv_positive~I_rainfall+Site, models[models$Rain_function==rainFun,]))$r.squared
  avg <- (mosq*n_mosq+denv*n_denv)/(n_mosq+n_denv)
  rFunsDFall[nrow(rFunsDFall)+1,] <- c(rainFun, round(mosq, 2), round(denv, 2), round(avg, 2))
}

write.csv(rFunsDFall, "Concatenated_Data/model_assessment/rainfall_functions_all.csv", row.names=F)

rFuns <- unique(models$Rain_function)
CNs <- unique(models$Country)
rFunsDFsep <- data.frame(matrix(nrow=0, ncol=5))
colnames(rFunsDFsep) <- c("Country", "Rain_function", "R2_mosq", "R2_denv", "R2_weighted_mean")

for (rainFun in rFuns){
  for (country in CNs){
    if (country == "Kenya"){
      n_mosq <- n_mosq_k
    } else {
      n_mosq <- n_mosq_e
    }
    mosq <- summary(lm(aedes_total~Mtot_rainfall+Site, models[models$Rain_function==rainFun & models$Country == country,]))$r.squared
    denv <- summary(lm(denv_positive~I_rainfall+Site, models[models$Rain_function==rainFun & models$Country == country,]))$r.squared
    avg <- (mosq*n_mosq+denv*n_denv)/(n_mosq+n_denv)
    rFunsDFsep[nrow(rFunsDFsep)+1,] <- c(country, rainFun, round(mosq, 2), round(denv, 2), round(avg, 2))
  }
}
rFunsDFsep <- rFunsDFsep[order(rFunsDFsep$Country),]
write.csv(rFunsDFsep, "Concatenated_Data/model_assessment/rainfall_functions_separated.csv", row.names=F)

# # R2 value for model using different rainfall functions
e <- subset(models, Country == "Ecuador" & Rain_function == "Quadratic")
k <- subset(models, Country == "Kenya" & Rain_function == "Briere")

rain2 <- rbind(e, k)
rain_final_denv <- summary(lm(denv_positive~I_rainfall+Site, rain2))$r.squared
rain_final_mosq <- summary(lm(aedes_total~Mtot_rainfall+Site, rain2))$r.squared

# Adjusted R2 calculations for temperature, rainfall, and humidity simulations ----------------------
# models <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_TRH_test_diff_hum_functions.csv", head=T, stringsAsFactors = F)
models <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_TRH_final_model.csv", head=T, stringsAsFactors = F)
models$Mtot_humidity <- models$M1 + models$M2 + models$M3
models$I_humidity <- models$I
models$Date <- as.Date(models$Date, "%Y-%m-%d")

# combine case data, vector data, and modeled data for temperature simulations
# merged_data_humidity <- merge(models[,c("Site", "Date", "I_humidity", "Mtot_humidity", "Humidity_function")], obs.data, by=c("Site", "Date"), all=T)
merged_data_humidity <- merge(models[,c("Site", "Date", "I_humidity", "Mtot_humidity")], obs.data, by=c("Site", "Date"), all=T)
# x <- subset(merged_data_humidity, Site == "Machala" & Date > "2014-01-01")
# d <- plot(x$Date, x$I_humidity, type='l')
# par(new = T)
# with(d, plot(x$Date, x$denv_positive, pch=16, axes=F, xlab=NA, ylab=NA, cex=1.2, type='b'), ylim=c(0,40))

# hum_lin_all_denv <- summary(lm(denv_positive~I_humidity+Site, merged_data_humidity[merged_data_humidity$Humidity_function=="linear",]))$r.squared
# hum_lin_all_mosq <- summary(lm(aedes_total~Mtot_humidity+Site, merged_data_humidity[merged_data_humidity$Humidity_function=="linear",]))$r.squared
# hum_lin_all_mean <- (hum_lin_all_denv*n_denv+hum_lin_all_mosq*n_mosq)/(n_denv+n_mosq)
# 
# hum_sig_all_denv <- summary(lm(denv_positive~I_humidity+Site, merged_data_humidity[merged_data_humidity$Humidity_function=="sigmoidal",]))$r.squared
# hum_sig_all_mosq <- summary(lm(aedes_total~Mtot_humidity+Site, merged_data_humidity[merged_data_humidity$Humidity_function=="sigmoidal",]))$r.squared
# hum_sig_all_mean <- (hum_sig_all_denv*n_denv+hum_sig_all_mosq*n_mosq)/(n_denv+n_mosq)

hum_denv <- summary(lm(denv_positive~I_humidity+Site, merged_data_humidity))$r.squared
hum_mosq <- summary(lm(aedes_total~Mtot_humidity+Site, merged_data_humidity))$r.squared

ggplot(merged_data_humidity, aes(x=Date, y=I_humidity, group=Site)) + geom_line()  + facet_wrap(~Site, scales="free", ncol=2)

# clinically suspected dengue
merged_data_humidity2 <- merge(models[,c("Site", "Date", "I_humidity", "Mtot_humidity")], cases, by=c("Site", "Date"), all=T)
merged_data_humidity2 <- subset(merged_data_humidity2, Site == "Machala"|Site == "Huaquillas"|Site == "Portovelo"|Site == "Zaruma")
cs_denv <- summary(lm(denv_positive~I_humidity+Site, merged_data_humidity2))$r.squared


# Other mosquito life stages ---------------------------------------------------
merged_data_2 <- merge(models[,c("Site", "Date", "Mtot_humidity")], vectors, by=c("Site", "Date"), all=T)

pupae <- summary(lm(pupae_total~Mtot_humidity+Site, merged_data_2))$r.squared
early <- summary(lm(early_instar_total~Mtot_humidity+Site, merged_data_2))$r.squared
late <- summary(lm(late_instar_total~Mtot_humidity+Site, merged_data_2))$r.squared
eggs <- summary(lm(egg_total_adjusted~Mtot_humidity+Site, merged_data_2))$r.squared

# Any arbovirus ----------------------------------------------------------------
load("Concatenated_Data/CDC_Zika_El_Oro_Total.RData")
cdcZika2$Site <- "Machala"
cases2 <- merge(cases, cdcZika2, by=c("Site", "Date"), all=T)
cases3 <- cases2[,c("denv_positive", "chikv_positive", "confirmed_cases_Zika")] #"chikv_positive_clinically_diagnosed", 
cases3$arboviruses <- rowSums(cases3, na.rm=T)
cases3$Date <- cases2$Date
cases3$Site <- cases2$Site
cases3$zikv_positive <- cases3$confirmed_cases_Zika

cases3 <- merge(cases3, merged_data_humidity[,c("Site", "Date", "I_humidity")], by=c("Site", "Date"))
arb_denv <- summary(lm(arboviruses~I_humidity+Site, cases3))$r.squared
