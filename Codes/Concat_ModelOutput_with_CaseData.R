#---------- concatenate SEI-SEIR modeled data with AIC case data -------
rm(list=ls()) #remove previous variable assignments

#------- load libraries -------
library(plyr)

# case data ------------
aic_cases <- read.csv("Kenya/Concatenated_Data/aic_timeline.csv", head=T, stringsAsFactors = F)
aic_cases[,c("visit_a_int_date", "infected.t0", "infected.t1")] <- lapply(aic_cases[,c("visit_a_int_date", "infected.t0", "infected.t1")], as.Date)

#------- temperature dependent simulation data -------
kisumu_seiseir_temp <- read.csv("Kenya/Concatenated_Data/SEI-SEIR/SEI-SEIR_Simulations_TempOnly_Kisumu.csv", head=T)
kisumu_seiseir_temp$Date <- as.Date(kisumu_seiseir_temp$Date, "%Y-%m-%d")

chulaimbo_seiseir_temp <- read.csv("Concatenated_Data/SEI-SEIR/SEI-SEIR_Simulations_TempOnly_Chulaimbo.csv", head=T)
chulaimbo_seiseir_temp$Date <- as.Date(chulaimbo_seiseir_temp$Date, "%Y-%m-%d")

msambweni_seiseir_temp <- read.csv("Concatenated_Data/SEI-SEIR/SEI-SEIR_Simulations_TempOnly_Msambweni.csv", head=T)
msambweni_seiseir_temp$Date <- as.Date(msambweni_seiseir_temp$Date, "%Y-%m-%d")

ukunda_seiseir_temp <- read.csv("Concatenated_Data/SEI-SEIR/SEI-SEIR_Simulations_TempOnly_Ukunda.csv", head=T)
ukunda_seiseir_temp$Date <- as.Date(ukunda_seiseir_temp$Date, "%Y-%m-%d")

#------- temperature independent simulation data -------
kisumu_seiseir_null <- read.csv("Concatenated_Data/SEI-SEIR/Null_Model_25.9C/SEI-SEIR_Simulations_25.9C_Kisumu.csv", head=T)
kisumu_seiseir_null$Date <- as.Date(kisumu_seiseir_null$Date, "%Y-%m-%d")

chulaimbo_seiseir_null <- read.csv("Concatenated_Data/SEI-SEIR/Null_Model_25.9C/SEI-SEIR_Simulations_25.9C_Chulaimbo.csv", head=T)
chulaimbo_seiseir_null$Date <- as.Date(chulaimbo_seiseir_null$Date, "%m/%d/%Y")

msambweni_seiseir_null <- read.csv("Concatenated_Data/SEI-SEIR/Null_Model_25.9C/SEI-SEIR_Simulations_25.9C_Msambweni.csv", head=T)
msambweni_seiseir_null$Date <- as.Date(msambweni_seiseir_null$Date, "%Y-%m-%d")

ukunda_seiseir_null <- read.csv("Concatenated_Data/SEI-SEIR/Null_Model_25.9C/SEI-SEIR_Simulations_25.9C_Ukunda.csv", head=T)
ukunda_seiseir_null$Date <- as.Date(ukunda_seiseir_null$Date, "%Y-%m-%d")

#---- concatenate presence absence data -------
case.model.df <- aic_cases
case.model.df$temp.model.Inf <- as.numeric(NA)
# case.model.df$null.model.Inf.Frac <- as.numeric(NA)

for (i in 1:nrow(aic_cases)){
  site <- substr(aic_cases$id_site[i], 1,1)
  if (site == "K"){
    tempModelData <- kisumu_seiseir_temp
    # nullModelData <- kisumu_seiseir_null
  } else if (site == "C"){
    tempModelData <- chulaimbo_seiseir_temp
    # nullModelData <- chulaimbo_seiseir_null
  } else if (site == "M"){
    tempModelData <- msambweni_seiseir_temp
    # nullModelData <- msambweni_seiseir_null
  } else if (site == "U"){
    tempModelData <- ukunda_seiseir_temp
    # nullModelData <- ukunda_seiseir_null
  }
  start <- case.model.df$infected.t0[i]
  stop <- case.model.df$infected.t1[i]
  temp_sub <- subset(tempModelData, Date >= start & Date <= stop)
  if (nrow(temp_sub)!=0){
    temp_expected_cases <- mean(temp_sub$I)
    case.model.df$temp.model.Inf[i] <- temp_expected_cases
    # null_sub <- subset(nullModelData, Date >= start & Date <= stop)
    # null_expected_cases <- mean(null_sub$I)/5
    # case.model.df$null.model.Inf.Frac[i] <- null_expected_cases
  } else if (nrow(temp_sub)==0){
    case.model.df$temp.model.Inf[i] <- NA
    # case.model.df$null.model.Inf.Frac[i] <- NA
  }
}

write.csv(case.model.df, "Concatenated_Data/AIC_Logistic_Regrssion_Data_all_tests.csv", row.names=F)

#---- concatenate data for paired t-test (before/after for single individual) -------
ttest <- subset(aic_cases, visit_a_infected_denv_stfd == 1 | visit_a_infected_chikv_stfd == 1)
ttest$before_model_frac <- as.numeric(NA)
ttest$after_model_frac  <- as.numeric(NA)

for (i in 1:nrow(ttest)){
  site <- substr(ttest$id_site[i], 1,1)
  if (site == "K"){
    tempModelData <- kisumu_seiseir_temp
  } else if (site == "C"){
    tempModelData <- chulaimbo_seiseir_temp
  } else if (site == "M"){
    tempModelData <- msambweni_seiseir_temp
  } else if (site == "U"){
    tempModelData <- ukunda_seiseir_temp
  }
  afterstart <- ttest$infected.t0[i]
  afterstop <- ttest$infected.t1[i]
  beforestop <- ttest$infected.t0[i] - 1
  beforestart <- beforestop - 6
  after_sub <- subset(tempModelData, Date >= afterstart & Date <= afterstop)
  before_sub <- subset(tempModelData, Date >= beforestart & Date <= beforestop)
  if (nrow(after_sub)!=0 & nrow(before_sub)!=0){
    after_expected_cases <- mean(after_sub$I)/5
    ttest$after_model_frac[i] <- after_expected_cases
    before_expected_cases <- mean(before_sub$I)/5
    ttest$before_model_frac[i] <- before_expected_cases
  } else {
    ttest$after_model_frac[i] <- NA
    ttest$before_model_frac[i] <- NA
  }
}

write.csv(ttest, "Concatenated_Data/AIC_ttest_data.csv", row.names=F)

#---- concatenate data for wilcoxin test with fraction of modeled data -------
case.model.df <- aic_cases
case.model.df$temp.model.Inf.Frac <- as.numeric(NA)

for (i in 1:nrow(aic_cases)){
  site <- substr(aic_cases$id_site[i], 1,1)
  if (site == "K"){
    tempModelData <- kisumu_seiseir_temp
    nullModelData <- kisumu_seiseir_null
  } else if (site == "C"){
    tempModelData <- chulaimbo_seiseir_temp
    nullModelData <- chulaimbo_seiseir_null
  } else if (site == "M"){
    tempModelData <- msambweni_seiseir_temp
    nullModelData <- msambweni_seiseir_null
  } else if (site == "U"){
    tempModelData <- ukunda_seiseir_temp
    nullModelData <- ukunda_seiseir_null
  }
  start <- case.model.df$infected.t0[i]
  stop <- case.model.df$infected.t1[i]
  temp_sub <- subset(tempModelData, Date >= start & Date <= stop)
  if (nrow(temp_sub)!=0){
    temp_expected_cases <- mean(temp_sub$I/(temp_sub$S + temp_sub$E + temp_sub$I + temp_sub$R))
    case.model.df$temp.model.Inf.Frac[i] <- temp_expected_cases
    null_sub <- subset(nullModelData, Date >= start & Date <= stop)
    null_expected_cases <- mean(null_sub$I/(null_sub$S + null_sub$E + null_sub$I + null_sub$R))
    case.model.df$null.model.Inf.Frac[i] <- null_expected_cases
  } else if (nrow(temp_sub)==0){
    case.model.df$temp.model.Inf.Frac[i] <- NA
    case.model.df$null.model.Inf.Frac[i] <- NA
  }
}

# write.csv(ttest, "Concatenated_Data/AIC_wilcox_proportion_model_data.csv", row.names=F)

# t-tests ---------------------
denv_ttest <- subset(ttest, visit_a_infected_denv_stfd == 1 & !is.na(before_model_frac))
t.test(denv_ttest$before_model_frac, denv_ttest$after_model_frac ,paired=TRUE)

chikv_ttest <- subset(ttest, visit_a_infected_chikv_stfd == 1 & !is.na(before_model_frac))
t.test(chikv_ttest$before_model_frac, chikv_ttest$after_model_frac ,paired=TRUE)

#---- concatenate data for regression with distributions -------
ttest <- subset(aic_cases, visit_a_infected_denv_stfd == 1 | visit_a_infected_chikv_stfd == 1)
ttest$before_model_frac <- as.numeric(NA)
ttest$after_model_frac  <- as.numeric(NA)

for (i in 1:nrow(ttest)){
  site <- substr(ttest$id_site[i], 1,1)
  if (site == "K"){
    tempModelData <- kisumu_seiseir_temp
  } else if (site == "C"){
    tempModelData <- chulaimbo_seiseir_temp
  } else if (site == "M"){
    tempModelData <- msambweni_seiseir_temp
  } else if (site == "U"){
    tempModelData <- ukunda_seiseir_temp
  }
  afterstart <- ttest$infected.t0[i]
  afterstop <- ttest$infected.t1[i]
  beforestop <- ttest$infected.t0[i] - 1
  beforestart <- beforestop - 6
  after_sub <- subset(tempModelData, Date >= afterstart & Date <= afterstop)
  before_sub <- subset(tempModelData, Date >= beforestart & Date <= beforestop)
  if (nrow(after_sub)!=0 & nrow(before_sub)!=0){
    after_expected_cases <- mean(after_sub$I)/5
    ttest$after_model_frac[i] <- after_expected_cases
    before_expected_cases <- mean(before_sub$I)/5
    ttest$before_model_frac[i] <- before_expected_cases
  } else {
    ttest$after_model_frac[i] <- NA
    ttest$before_model_frac[i] <- NA
  }
}

write.csv(ttest, "Concatenated_Data/AIC_ttest_data.csv", row.names=F)



# dataset <- data.frame()
# for (h in 1:8){
#   if (h == 1){
#     caseData <- kisumu_cases_D
#     modelData <- kisumu_seiseir
#   } else if (h == 2){
#     caseData <- chulaimbo_cases_D
#     modelData <- chulaimbo_seiseir
#   } else if (h == 3){
#     caseData <- msambweni_cases_D
#     modelData <- msambweni_seiseir
#   } else if (h == 4){
#     caseData <- ukunda_cases_D
#     modelData <- ukunda_seiseir
#   } else if (h == 5){
#     caseData <- kisumu_cases_C
#     modelData <- kisumu_seiseir
#   } else if (h == 6){
#     caseData <- chulaimbo_cases_C
#     modelData <- chulaimbo_seiseir
#   } else if (h == 7){
#     caseData <- msambweni_cases_C
#     modelData <- msambweni_seiseir
#   } else if (h == 8){
#     caseData <- ukunda_cases_C
#     modelData <- ukunda_seiseir
#   }
#   caseData$model.Inf.Frac <- as.numeric(NA)
#   caseData$model.Ave.Inf.Frac <- as.numeric(NA)
#   for (i in 1:nrow(caseData)){
#     start <- caseData[i,"infected.t0"]
#     stop <- caseData[i,"infected.t1"]
#     sub <- subset(modelData, Date >= start & Date <= stop)
#     sub$model.I.Fraction <- sub$I/(sub$S + sub$E + sub$I + sub$R)
#     infFrac <- sum(sub$model.I.Fraction)
#     numDays <- as.numeric(difftime(stop, start, units = c("days")))
#     AveInfFrac <- infFrac/numDays
#     caseData[i, "model.Inf.Frac"] <- infFrac
#     caseData[i, "model.Ave.Inf.Frac"] <- AveInfFrac
#   }
#   dataset <- rbind(dataset, caseData)
# }
# write.csv(dataset, "Concatenated_Data/AIC_Logistic_Regrssion_Data.csv", row.names=F)
# write.csv(dataset, "Concatenated_Data/AIC_Logistic_Regrssion_NullData.csv", row.names=F)


#--------------------------------------------
#--------------------------------------------
#---- concatenate data for regression
library(plyr)
caseData2 <- aic_cases
caseData2$month <- as.numeric(format(caseData2$interview_date_aic,"%m"))
caseData2$year <- as.numeric(format(caseData2$interview_date_aic,"%Y"))
caseData2$timePeriod <- as.character(NA)

for (i in 1:nrow(caseData2)){
    if (caseData2$month[i] < 4){
    caseData2$timePeriod[i] <- "1-JFM"  
  } else if (caseData2$month[i] >= 4 & caseData2$month[i] <= 6){
    caseData2$timePeriod[i] <- "2-AMJ"  
  } else if (caseData2$month[i] >= 7 & caseData2$month[i] <= 9){
    caseData2$timePeriod[i] <- "3-JAS"  
  } else if (caseData2$month[i] >= 10){
    caseData2$timePeriod[i] <- "4-OND"
  }
}

df <- ddply(caseData2, .(year, timePeriod, cohort, disease, hospital_site, test),
            summarise,
            caseFraction = sum(na.omit(result == 1))/length(result),
            earliestDate = min(infected.t0),
            latestDate = max(infected.t1))

df$model.Inf.Frac <- as.numeric(NA)
df$model.Ave.Inf.Frac <- as.numeric(NA)

for (i in 1:nrow(df)){
    start <- df[i,"earliestDate"]
    stop <- df[i,"latestDate"]
    if (df$hospital_site[i] == 1){
      modelData2 <- kisumu_seiseir
    } else if (df$hospital_site[i] == 2){
      modelData2 <- chulaimbo_seiseir
    } else if (df$hospital_site[i] == 3){
      modelData2 <- msambweni_seiseir
    } else {
      modelData2 <- ukunda_seiseir
    }
    sub <- subset(modelData2, Date >= start & Date <= stop)
    sub$model.I.Fraction <- sub$I/(sub$S + sub$E + sub$I + sub$R)
    infFrac <- sum(sub$model.I.Fraction)
    numDays <- as.numeric(difftime(stop, start, units = c("days")))
    AveInfFrac <- infFrac/numDays
    df[i, "model.Inf.Frac"] <- infFrac
    df[i, "model.Ave.Inf.Frac"] <- AveInfFrac
}
write.csv(df, "Concatenated_Data/AIC_Regrssion_Data.csv", row.names=F)

lm(df$caseFraction ~ df$model.Inf.Frac + df$hospital_site + df$disease + df$test)
test <- subset(df, disease == "chikv" & test == "IgG")
plot(test$caseFraction, test$model.Inf.Frac)
plot(test$caseFraction, test$model.Ave.Inf.Frac)
lm(test$caseFraction ~ test$model.Inf.Frac + test$hospital_site)
