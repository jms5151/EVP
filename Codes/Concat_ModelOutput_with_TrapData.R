#---------- concatenate SEI-SEIR modeled data with aedes aegypti trap data
rm(list=ls()) #remove previous variable assignments

#---- read in simulated model data
kisumu_seiseir <- read.csv("Concatenated_Data/SEI-SEIR/SEI-SEIR_Simulations_TempOnly_Kisumu.csv", head=T, as.is=T)
chulaimbo_seiseir <- read.csv("Concatenated_Data/SEI-SEIR/SEI-SEIR_Simulations_TempOnly_Chulaimbo.csv", head=T, as.is=T)
msambweni_seiseir <- read.csv("Concatenated_Data/SEI-SEIR/SEI-SEIR_Simulations_TempOnly_Msambweni.csv", head=T, as.is=T)
ukunda_seiseir <- read.csv("Concatenated_Data/SEI-SEIR/SEI-SEIR_Simulations_TempOnly_Ukunda.csv", head=T, as.is=T)

#---- read in simulated null model data 25.9C
# kisumu_seiseir <- read.csv("Concatenated_Data/SEI-SEIR/Null_Model_25.9C/SEI-SEIR_Simulations_25.9C_Kisumu.csv", head=T, as.is=T)
# chulaimbo_seiseir <- read.csv("Concatenated_Data/SEI-SEIR/Null_Model_25.9C/SEI-SEIR_Simulations_25.9C_Chulaimbo.csv", head=T, as.is=T)
# msambweni_seiseir <- read.csv("Concatenated_Data/SEI-SEIR/Null_Model_25.9C/SEI-SEIR_Simulations_25.9C_Msambweni.csv", head=T, as.is=T)
# ukunda_seiseir <- read.csv("Concatenated_Data/SEI-SEIR/Null_Model_25.9C/SEI-SEIR_Simulations_25.9C_Ukunda.csv", head=T, as.is=T)

#---- read in simulated null model data 29C
kisumu_seiseir <- read.csv("Concatenated_Data/SEI-SEIR/Null_Model_25.9C/SEI-SEIR_Simulations_29C_Kisumu.csv", head=T, as.is=T)
chulaimbo_seiseir <- read.csv("Concatenated_Data/SEI-SEIR/Null_Model_25.9C/SEI-SEIR_Simulations_29C_Chulaimbo.csv", head=T, as.is=T)
msambweni_seiseir <- read.csv("Concatenated_Data/SEI-SEIR/Null_Model_25.9C/SEI-SEIR_Simulations_29C_Msambweni.csv", head=T, as.is=T)
ukunda_seiseir <- read.csv("Concatenated_Data/SEI-SEIR/Null_Model_25.9C/SEI-SEIR_Simulations_29C_Ukunda.csv", head=T, as.is=T)

#---- read in vector data
bg <- read.csv("Concatenated_Data/vector/bg.csv", head=T, as.is=T)
hlc <- read.csv("Concatenated_Data/vector/hlc.csv", head=T, as.is=T)
larvae <- read.csv("Concatenated_Data/vector/larvae.csv", head=T, as.is=T)
ovitrap <- read.csv("Concatenated_Data/vector/ovitrap.csv", head=T, as.is=T)
prokopak <- read.csv("Concatenated_Data/vector/prokopak.csv", head=T, as.is=T)

#---- concatenate data for regression
# library(plyr)
require(pracma)

#-- bg traps
df.bg <- bg

for (i in 1:nrow(df.bg)){
  start <- df.bg[i,"dropoffDate"]
  stop <- df.bg[i,"pickupDate"]
  startMonth <- as.character(as.Date(stop) - 28)
  if (df.bg$study_site[i] == "Kisumu"){
    modelData2 <- kisumu_seiseir
  } else if (df.bg$study_site[i] == "Chulaimbo"){
    modelData2 <- chulaimbo_seiseir
  } else if (df.bg$study_site[i] == "Msambweni"){
    modelData2 <- msambweni_seiseir
  } else {
    modelData2 <- ukunda_seiseir
  }
  wk.sub <- subset(modelData2, Date >= start & Date <= stop)
  # wk.sub$model.M3.Frac <- wk.sub$M3/wk.sub$totalMosquitoPop
  wk.Total.auc = trapz(wk.sub$totalMosquitoPop) # total is same as fraction
  df.bg[i, "weekly.model.Total"] <- wk.Total.auc
  mn.sub <- subset(modelData2, Date >= startMonth & Date <= stop)
  # mn.sub$model.M3.Frac <- mn.sub$M3/mn.sub$totalMosquitoPop
  mn.Total.auc = trapz(mn.sub$totalMosquitoPop) # total is same as fraction
  df.bg[i, "monthly.model.Total"] <- mn.Total.auc
}

write.csv(df.bg, "Concatenated_Data/BG_Regrssion_Data.csv", row.names=F)
# write.csv(df.bg, "Concatenated_Data/BG_Regrssion_Data_Null_25.9C.csv", row.names=F)
# write.csv(df.bg, "Concatenated_Data/BG_Regrssion_Data_Null_29C.csv", row.names=F)

#-- hlc data
df.hlc <- hlc

for (i in 1:nrow(df.hlc)){
  start <- df.hlc[i,"start_date"]
  stop <- df.hlc[i,"end_date"]
  startMonth <- as.character(as.Date(stop) - 28)
  if (df.hlc$study_site[i] == "Kisumu"){
    modelData2 <- kisumu_seiseir
  } else if (df.hlc$study_site[i] == "Chulaimbo"){
    modelData2 <- chulaimbo_seiseir
  } else if (df.hlc$study_site[i] == "Msambweni"){
    modelData2 <- msambweni_seiseir
  } else {
    modelData2 <- ukunda_seiseir
  }
  wk.sub <- subset(modelData2, Date >= start & Date <= stop)
  wk.Total.auc = trapz(wk.sub$totalMosquitoPop) # total is same as fraction
  df.hlc[i, "weekly.model.Total"] <- wk.Total.auc
  mn.sub <- subset(modelData2, Date >= startMonth & Date <= stop)
  mn.Total.auc = trapz(mn.sub$totalMosquitoPop) # total is same as fraction
  df.hlc[i, "monthly.model.Total"] <- mn.Total.auc
}

write.csv(df.hlc, "Concatenated_Data/HLC_Regrssion_Data.csv", row.names=F)
# write.csv(df.hlc, "Concatenated_Data/HLC_Regrssion_Data_Null_25.9C.csv", row.names=F)
# write.csv(df.hlc, "Concatenated_Data/HLC_Regrssion_Data_Null_29C.csv", row.names=F)

#-- larvae data
df.lar <- larvae

for (i in 1:nrow(df.lar)){
  stop <- df.lar[i,"Date"]
  start <- as.character(as.Date(stop) - 7)
  startMonth <- as.character(as.Date(stop) - 28)
  if (df.lar$study_site[i] == "Kisumu"){
    modelData2 <- kisumu_seiseir
  } else if (df.lar$study_site[i] == "Chulaimbo"){
    modelData2 <- chulaimbo_seiseir
  } else if (df.lar$study_site[i] == "Msambweni"){
    modelData2 <- msambweni_seiseir
  } else {
    modelData2 <- ukunda_seiseir
  }
  wk.sub <- subset(modelData2, Date >= start & Date <= stop)
  wk.Total.auc = trapz(wk.sub$totalMosquitoPop) # total is same as fraction
  df.lar[i, "weekly.model.Total"] <- wk.Total.auc
  mn.sub <- subset(modelData2, Date >= startMonth & Date <= stop)
  mn.Total.auc = trapz(mn.sub$totalMosquitoPop) # total is same as fraction
  df.lar[i, "monthly.model.Total"] <- mn.Total.auc
}

write.csv(df.lar, "Concatenated_Data/Larvae_Regrssion_Data.csv", row.names=F)
# write.csv(df.lar, "Concatenated_Data/Larvae_Regrssion_Data_Null_25.9C.csv", row.names=F)
# write.csv(df.lar, "Concatenated_Data/Larvae_Regrssion_Data_Null_29C.csv", row.names=F)

#-- ovitrap data
df.ovi <- ovitrap

for (i in 1:nrow(df.ovi)){
  start <- df.ovi[i,"date_set"]
  stop <- df.ovi[i,"date_collected"]
  startMonth <- as.character(as.Date(stop) - 28)
  if (df.ovi$study_site[i] == "Kisumu"){
    modelData2 <- kisumu_seiseir
  } else if (df.ovi$study_site[i] == "Chulaimbo"){
    modelData2 <- chulaimbo_seiseir
  } else if (df.ovi$study_site[i] == "Msambweni"){
    modelData2 <- msambweni_seiseir
  } else {
    modelData2 <- ukunda_seiseir
  }
  wk.sub <- subset(modelData2, Date >= start & Date <= stop)
  wk.Total.auc = trapz(wk.sub$totalMosquitoPop) # total is same as fraction
  df.ovi[i, "weekly.model.Total"] <- wk.Total.auc
  mn.sub <- subset(modelData2, Date >= startMonth & Date <= stop)
  mn.Total.auc = trapz(mn.sub$totalMosquitoPop) # total is same as fraction
  df.ovi[i, "monthly.model.Total"] <- mn.Total.auc
}

write.csv(df.ovi, "Concatenated_Data/Ovitrap_Regrssion_Data.csv", row.names=F)
# write.csv(df.ovi, "Concatenated_Data/Ovitrap_Regrssion_Data_Null_25.9C.csv", row.names=F)
# write.csv(df.ovi, "Concatenated_Data/Ovitrap_Regrssion_Data_Null_29C.csv", row.names=F)

#-- prokopak data
df.pro <- prokopak

for (i in 1:nrow(df.pro)){
  stop <- df.pro[i,"Date"]
  start <- as.character(as.Date(stop) - 7)
  startMonth <- as.character(as.Date(stop) - 28)
  if (df.pro$study_site[i] == "Kisumu"){
    modelData2 <- kisumu_seiseir
  } else if (df.pro$study_site[i] == "Chulaimbo"){
    modelData2 <- chulaimbo_seiseir
  } else if (df.pro$study_site[i] == "Msambweni"){
    modelData2 <- msambweni_seiseir
  } else {
    modelData2 <- ukunda_seiseir
  }
  wk.sub <- subset(modelData2, Date >= start & Date <= stop)
  wk.Total.auc = trapz(wk.sub$totalMosquitoPop) # total is same as fraction
  df.pro[i, "weekly.model.Total"] <- wk.Total.auc
  mn.sub <- subset(modelData2, Date >= startMonth & Date <= stop)
  mn.Total.auc = trapz(mn.sub$totalMosquitoPop) # total is same as fraction
  df.pro[i, "monthly.model.Total"] <- mn.Total.auc
}

write.csv(df.pro, "Concatenated_Data/Prokopak_Regrssion_Data.csv", row.names=F)
# write.csv(df.pro, "Concatenated_Data/Prokopak_Regrssion_Data_Null_25.9C.csv", row.names=F)
# write.csv(df.pro, "Concatenated_Data/Prokopak_Regrssion_Data_Null_29C.csv", row.names=F)
