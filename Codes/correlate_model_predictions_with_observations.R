# calculate correlations between model predictions and observations ---------------------------
rm(list=ls()) #remove previous variable assignments

# load case data
load("Concatenated_Data/case_data/merged_case_data.RData")

# load vector data
load("Concatenated_Data/vector_data/merged_vector_data.RData")

# combine observed case and vector data
obs.data <- merge(cases[,c("Site", "Date", "denv_positive")], vectors[,c("Site", "Date", "aedes_total")], by=c("Site", "Date"), all=T)

# combine modeled data with observed data
rmodels <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_THR_test_diff_rain_functions.csv", head=T, stringsAsFactors=F)
rmodels$Mtot <- rmodels$M1 + rmodels$M2 + rmodels$M3
rmodels$Date <- as.Date(rmodels$Date, "%Y-%m-%d")
merged_data <- merge(rmodels, obs.data, by=c("Site", "Date"))

# correlation site & by rainfall function ----------------------------------------------------
sites <- unique(rmodels$Site)
r_functions <- c("Briere", "Quadratic", "Inverse")
df_rain <- data.frame(matrix(ncol=4, nrow=0))
colnames(df_rain) <- c("Site", "Rain_function", "Aedes_corr", "Dengue_corr")

for (j in 1:length(sites)){
  for (k in 1:length(r_functions)){
    aedes <- cor(merged_data$aedes_total[merged_data$Site==sites[j]&merged_data$Rain_function==r_functions[k]], merged_data$Mtot[merged_data$Site==sites[j]&merged_data$Rain_function==r_functions[k]], use = "complete.obs")
    dengue <- cor(merged_data$denv_positive[merged_data$Site==sites[j]&merged_data$Rain_function==r_functions[k]], merged_data$I[merged_data$Site==sites[j]&merged_data$Rain_function==r_functions[k]], use = "complete.obs")
    df_rain[(nrow(df_rain)+1),] <- c(sites[j], r_functions[k], round(aedes,2), round(dengue,2))
  }
}

write.csv(df_rain, "Concatenated_Data/model_assessment/correlation_rainfall_functions_separated.csv", row.names=F)

# subset results to the best models ----------------------------------------------------------
library(tidyverse)

corr_aedes <- df_rain %>% 
  group_by(Site) %>%
  filter(Aedes_corr == max(as.numeric(Aedes_corr))) %>%
  select(-Dengue_corr)

write.csv(corr_aedes, "Concatenated_Data/model_assessment/correlation_best_mod_aedes.csv", row.names=F)

corr_dengue <- df_rain %>% 
  group_by(Site) %>%
  filter(Dengue_corr == max(as.numeric(Dengue_corr))) %>%
  select(-Aedes_corr)
corr_dengue <- corr_dengue[1:8,] # remove last two rows based on aedes correlation

write.csv(corr_dengue, "Concatenated_Data/model_assessment/correlation_best_mod_dengue.csv", row.names=F)

# mean of means
mean(as.numeric(corr_aedes$Aedes_corr))
mean(as.numeric(corr_dengue$Dengue_corr))

# other mosquito life stages -----------------------------------------------------------------
merged_data_vectors <- merge(rmodels, vectors, by=c("Site", "Date"))

# overall mean
vectors <- data.frame(matrix(ncol=6, nrow=0))
colnames(vectors) <- c("Site", "Rain_function", "Pupae", "Late_instars", "Early_instars", "Eggs")

for (j in 1:length(sites)){
  for (k in 1:length(r_functions)){
    pupae <- cor(merged_data_vectors$pupae_total[merged_data_vectors$Site==sites[j]&merged_data_vectors$Rain_function==r_functions[k]], merged_data_vectors$Mtot[merged_data_vectors$Site==sites[j]&merged_data_vectors$Rain_function==r_functions[k]], use = "complete.obs")
    lateInstars <- cor(merged_data_vectors$late_instar_total[merged_data_vectors$Site==sites[j]&merged_data_vectors$Rain_function==r_functions[k]], merged_data_vectors$Mtot[merged_data_vectors$Site==sites[j]&merged_data_vectors$Rain_function==r_functions[k]], use = "complete.obs")
    earlyInstars <- cor(merged_data_vectors$early_instar_total[merged_data_vectors$Site==sites[j]&merged_data_vectors$Rain_function==r_functions[k]], merged_data_vectors$Mtot[merged_data_vectors$Site==sites[j]&merged_data_vectors$Rain_function==r_functions[k]], use = "complete.obs")
    eggs <- cor(merged_data_vectors$egg_total_adjusted[merged_data_vectors$Site==sites[j]&merged_data_vectors$Rain_function==r_functions[k]], merged_data_vectors$Mtot[merged_data_vectors$Site==sites[j]&merged_data_vectors$Rain_function==r_functions[k]], use = "complete.obs")
    vectors[(nrow(vectors)+1),] <- c(sites[j], r_functions[k], round(pupae,2), round(lateInstars,2), round(earlyInstars,2), round(eggs,2))
  }
}

write.csv(vectors, "Concatenated_Data/model_assessment/correlation_rainfall_functions_other_mosquito_life_stages.csv", row.names=F)

# any arbovirus ------------------------------------------------------------------------------
load("Concatenated_Data/CDC_Zika_El_Oro_Total.RData")
cdcZika2$Site <- "Machala"
cases2 <- merge(cases, cdcZika2, by=c("Site", "Date"), all=T)
cases3 <- cases2[,c("denv_positive", "chikv_positive", "confirmed_cases_Zika")] 
cases3$arboviruses <- rowSums(cases3, na.rm=T)
cases3$Date <- cases2$Date
cases3$Site <- cases2$Site
cases3$zikv_positive <- cases3$confirmed_cases_Zika
merged_data_cases <- merge(cases3, merged_data, by=c("Site", "Date"))
merged_data_cases <- subset(merged_data_cases, Site == "Machala" | Site == "Huaquillas")
sites <- unique(merged_data_cases$Site)
arbos <- data.frame(matrix(ncol=3, nrow=0))
colnames(arbos) <- c("Site", "Rain_function", "Arboviruses")

for(j in 1:length(sites)){
  for (k in 1:length(r_functions)){
    arboviruses <- cor(merged_data_cases$arboviruses[merged_data_cases$Site==sites[j]&merged_data_cases$Rain_function==r_functions[k]], merged_data_cases$I[merged_data_cases$Site==sites[j]&merged_data_cases$Rain_function==r_functions[k]], use = "complete.obs")
    arbos[(nrow(arbos)+1),] <- c(sites[j], r_functions[k], round(arboviruses,2))
  }
}

write.csv(vectors, "Concatenated_Data/model_assessment/correlation_rainfall_functions_all_arboviruses.csv", row.names=F)