rm(list=ls()) #remove previous variable assignments

# packages -----------------------------------------------------------------
library(plyr)

# source redcap data --------------------------------------------------------
source("Codes/REDCap_extract_case_data.R")

#remove extraneous rows
R01_lab_results <- R01_lab_results[which(!is.na(R01_lab_results$redcap_event_name))  , ]
R01_lab_results <- R01_lab_results[which(R01_lab_results$redcap_event_name!="visit_a2_arm_1"&R01_lab_results$redcap_event_name!="visit_b2_arm_1"&R01_lab_results$redcap_event_name!="visit_c2_arm_1"&R01_lab_results$redcap_event_name!="visit_d2_arm_1"&R01_lab_results$redcap_event_name!="visit_c2_arm_1"&R01_lab_results$redcap_event_name!="visit_u24_arm_1"&R01_lab_results$redcap_event_name!="patient_informatio_arm_1")  , ]

# format corhort labels
R01_lab_results$id_cohort<-substr(R01_lab_results$person_id, 2, 2) #F and M are AIC, 0 C and D are other
R01_lab_results <- within(R01_lab_results, id_cohort[R01_lab_results$id_cohort=="M"] <- "F")

# combine cohort data 
R01_lab_results$sex <- ifelse(!is.na(R01_lab_results$gender_aic), R01_lab_results$gender_aic, R01_lab_results$gender) # 0=Male, 1=Female
R01_lab_results$school <- ifelse(!is.na(R01_lab_results$occupation_aic), R01_lab_results$occupation_aic, R01_lab_results$occupation)
R01_lab_results$home_village <- ifelse(!is.na(R01_lab_results$village_aic), R01_lab_results$village_aic, R01_lab_results$village)

# Create a new variable by studyID for study site
R01_lab_results$id_city <- substr(R01_lab_results$person_id, 1, 1) #C is Chulaimbo, K is Kisumu, M is Msambweni, U is Ukunda, one 0 not sure, R is also Chulaimbo, G stands for Nganja (one of the subparts of Msambweni), L is for Mililani (part of Msambweni)
R01_lab_results$Site <- NA
R01_lab_results <- within (R01_lab_results, Site[R01_lab_results$id_city=="C" | R01_lab_results$id_city=="R"] <- "Chulaimbo")
R01_lab_results <- within (R01_lab_results, Site[R01_lab_results$id_city=="K"] <- "Kisumu")
R01_lab_results <- within (R01_lab_results, Site[R01_lab_results$id_city=="M" | R01_lab_results$id_city=="G" | R01_lab_results$id_city=="L"] <- "Msambweni")
R01_lab_results <- within (R01_lab_results, Site[R01_lab_results$id_city=="U" ] <- "Ukunda")

# Remove the one studyID starting with a O
R01_lab_results <- R01_lab_results[which(R01_lab_results$id_city!="O"), ]

# interview dates ---------------------------------------------------------
R01_lab_results$Date <- paste0(R01_lab_results$interview_date_aic, R01_lab_results$interview_date)
R01_lab_results$Date <- as.Date(R01_lab_results$Date, "%Y-%m-%d")
R01_lab_results$Date[R01_lab_results$Date == "1900-01-01"] <- NA

# fever -------------------------------------------------------------------
R01_lab_results$temp[R01_lab_results$temp == 385] <- 38.5
R01_lab_results$fever_by_temp <- ifelse(R01_lab_results$temp >=38.0 & R01_lab_results$temp < 43, 1, 0)
R01_lab_results$Symptoms_all_cohorts <- paste0(R01_lab_results$symptoms_aic, R01_lab_results$symptoms_aic)
R01_lab_results$Symptoms_all_cohorts<-tolower(R01_lab_results$Symptoms_all_cohorts)
R01_lab_results$fever_by_symptoms<-grepl("fever", R01_lab_results$Symptoms_all_cohorts)
R01_lab_results$fever_by_symptoms <- ifelse(R01_lab_results$fever_by_symptoms==TRUE, 1, 0)
R01_lab_results$fever <- R01_lab_results$fever_by_temp + R01_lab_results$fever_by_symptoms
R01_lab_results$fever <- ifelse(R01_lab_results$fever>=1,1,0)

# age ---------------------------------------------------------------------
R01_lab_results$age = R01_lab_results$age_calc_rc  # your new merged column starts with age_calc_rc
R01_lab_results$age[!is.na(R01_lab_results$aic_calculated_age)] = R01_lab_results$aic_calculated_age[!is.na(R01_lab_results$aic_calculated_age)]  # merge with aic_calculated_age
R01_lab_results$age[!is.na(R01_lab_results$age_calc)] = R01_lab_results$age_calc[!is.na(R01_lab_results$age_calc)]  # merge with age_calc
R01_lab_results$age<-round(R01_lab_results$age)

# age group
# R01_lab_results$age_group<-NA
# R01_lab_results <- within(R01_lab_results, age_group[age<=2] <- "under 2")
# R01_lab_results <- within(R01_lab_results, age_group[age>2 & age<=5] <- "2-5")
# R01_lab_results <- within(R01_lab_results, age_group[age>5 & age<=10] <- "6-10")
# R01_lab_results <- within(R01_lab_results, age_group[age>10 & age<=15] <- "11-15")
# R01_lab_results <- within(R01_lab_results, age_group[age>15] <- "over 15")
# R01_lab_results$age_group <- factor(R01_lab_results$age_group, levels = c("under 2", "2-5", "6-10", "11-15", "over 15"))

# serotype information ---------------------------------------------------
serotypes <- which(names(R01_lab_results)%in%c("serotype_pcr_denv_kenya___1", "serotype_pcr_denv_kenya___2", "serotype_pcr_denv_kenya___3", "serotype_pcr_denv_kenya___4", "serotype_pcr_denv_kenya___99", "team_leader_larva___99", "team_leader_other_larva"))
R01_lab_results$Serotype <- ifelse(is.na(R01_lab_results$serotype_pcr_denv_kenya___1), NA, names(R01_lab_results[,c(serotypes)])[which(R01_lab_results[,c(serotypes)] == 1, arr.ind=T)[, "col"]])
R01_lab_results$Serotype <- mapvalues(R01_lab_results$Serotype, from=c("serotype_pcr_denv_kenya___1", "serotype_pcr_denv_kenya___2", "serotype_pcr_denv_kenya___3", "serotype_pcr_denv_kenya___4"), to=c(1,2,3,4))

# make NAs for denv and chikv test results zeros 
labtests <- which(names(R01_lab_results)%in%c("result_igg_denv_stfd", "result_pcr_denv_kenya", "result_pcr_denv_stfd", "denv_result_ufi", "result_igg_chikv_stfd", "result_pcr_chikv_kenya", "result_pcr_chikv_stfd", "chikv_result_ufi", "ab_denv_stfd_igg", "bc_denv_stfd_igg", "cd_denv_stfd_igg", "de_denv_stfd_igg", "ef_denv_stfd_igg", "fg_denv_stfd_igg", "gh_denv_stfd_igg", "ab_chikv_stfd_igg", "bc_chikv_stfd_igg", "cd_chikv_stfd_igg", "de_chikv_stfd_igg", "ef_chikv_stfd_igg", "fg_chikv_stfd_igg", "gh_chikv_stfd_igg"))
R01_lab_results[,labtests][is.na(R01_lab_results[,labtests])] <- 0

# add seroconversion for igg at last visit (none yet)
R01_lab_results$hi_denv_stfd_igg <- 0
R01_lab_results$hi_chikv_stfd_igg <- 0

# create dataframe for infectious period for all kids
visitNames <- c("a", "b", "c", "d", "e", "f", "g", "h", "i")

for (i in 1:nrow(R01_lab_results)){
  visit <- substr(R01_lab_results$redcap_event_name[i], 7,7)
  d.seroconv <- paste0(visit, visitNames[which(visitNames==visit)+1], "_denv_stfd_igg")
  R01_lab_results$denv[i] <- ifelse(R01_lab_results[i,d.seroconv]==1|R01_lab_results$result_pcr_denv_kenya[i]==1|R01_lab_results$result_pcr_denv_stfd[i]==1|R01_lab_results$denv_result_ufi[i]==1, 1, 0)
  c.seroconv <- paste0(visit, visitNames[which(visitNames==visit)+1], "_chikv_stfd_igg")
  R01_lab_results$chikv[i] <- ifelse(R01_lab_results[i,c.seroconv]==1|R01_lab_results$result_pcr_chikv_kenya[i]==1|R01_lab_results$result_pcr_chikv_stfd[i]==1|R01_lab_results$chikv_result_ufi[i]==1, 1, 0)
}

# sum(R01_lab_results$denv==1)
# sum(R01_lab_results$chikv==1)

casedata <- R01_lab_results[,c("id_cohort", "person_id", "Site", "redcap_event_name"
                               , "Date", "date_symptom_onset", "age", "gender"
                               , "school", "home_village", "child_travel", "where_travel"
                               , "where_travel_aic1", "where_travel_aic2", "where_travel_aic3"
                               , "where_travel_aic4", "stay_overnight_aic", "temp"
                               , "fever_by_symptoms", "fever", "fever_contact"
                               , "denv", "chikv", "Serotype")]
                               # , "result_igg_denv_stfd", "result_igm_denv_stfd"
                               # , "result_pcr_denv_kenya", "result_pcr_denv_stfd"
                               # , "denv_result_ufi", "result_igg_chikv_stfd"
                               # , "result_igm_chikv_stfd", "result_pcr_chikv_kenya"
                               # , "result_pcr_chikv_stfd", "chikv_result_ufi"
                               # , "ab_denv_stfd_igg", "bc_denv_stfd_igg"
                               # , "cd_denv_stfd_igg", "de_denv_stfd_igg", "ef_denv_stfd_igg"
                               # , "fg_denv_stfd_igg", "gh_denv_stfd_igg", "ab_chikv_stfd_igg"
                               # , "bc_chikv_stfd_igg", "cd_chikv_stfd_igg", "de_chikv_stfd_igg"
                               # , "ef_chikv_stfd_igg", "fg_chikv_stfd_igg", "gh_chikv_stfd_igg")]

# subset by cohort ---------------------------------------------------------
hcc <- subset(casedata, id_cohort == "C")
aic <- subset(casedata, id_cohort == "F")

write.csv(hcc, "Kenya/Concatenated_Data/hcc_results.csv", row.names = F)

# exposure and infectious periods for aic ----------------------------------
# all aic kids are febrile so if days with symptoms is na replace with zero as they had fever on day of visit 
aic$date_symptom_onset[is.na(aic$date_symptom_onset)] <- 0

# if kids had fever for more than 7 days treat as day 0
aic$date_symptom_onset[aic$date_symptom_onset > 7] <- 0

# exposure and infection start and end dates
aic$date_start_infectious <- (aic$Date - aic$date_symptom_onset)
aic$date_end_infectious <- aic$date_start_infectious + 4
aic$date_start_exposed <- aic$date_start_infectious - 7
aic$date_end_exposed <- aic$date_start_infectious - 1

aic$Year.Month <- format(aic$date_start_infectious, "%Y-%m")
write.csv(aic, "Kenya/Concatenated_Data/aic_results.csv", row.names = F)

# aggregate aic data -------------------------------------------------------
epimonths <- ddply(aic, .(Site, Year.Month), summarise, denv_positive = sum(denv==1), denv_negative = sum(denv==0), chikv_positive = sum(chikv==1), chikv_negative = sum(chikv==0))
epimonths <- epimonths[complete.cases(epimonths),]
  
# save epimonth data
write.csv(epimonths, "Kenya/Concatenated_Data/aic_cases_by_month.csv", row.names = F)




# assemble exposure dates ---------------------------------------------------
# library(dplyr)
# library(ggplot2)
# library(zoo)
# library(lubridate)
# library(tidyr)
# 
# aic.df <- subset(aic.tsi.foi, !is.na(interview_date_aic))
# viruses <- c("denv", "chikv")
# peopIDs <- unique(aic.df$person_id)
# aic.dates <- seq.Date(min(aic.df$interview_date_aic), max(aic.df$interview_date_aic), "days") 
# 
# for (i in 1:length(viruses)){
#   vir <- as.data.frame(aic.dates)
#   if (viruses[i]=="denv"){
#     tempdat <- aic.df[, grepl("person_id|date|site|denv", names(aic.df) ) ]
#   } else {
#     tempdat <- aic.df[, grepl("person_id|date|site|chikv", names(aic.df) ) ]
#   }
#   for (j in 1:length(peopIDs)){
#     visits <- subset(tempdat, person_id == peopIDs[j])
#     for (k in 1:nrow(visits)){
#       # x<-colnames(visits)[grepl("result", colnames(visits))]
#       # visits$positive <- ifelse(visits[j,7]==1|visits[j,8]==1|visits[j,9]==1|)
#     }
#   }
# }
#   
# #remove extraneous rows
# R01_lab_results<- R01_lab_results[which(!is.na(R01_lab_results$redcap_event_name))  , ]
# R01_lab_results<- R01_lab_results[which(R01_lab_results$redcap_event_name!="visit_a2_arm_1"&R01_lab_results$redcap_event_name!="visit_b2_arm_1"&R01_lab_results$redcap_event_name!="visit_c2_arm_1"&R01_lab_results$redcap_event_name!="visit_d2_arm_1"&R01_lab_results$redcap_event_name!="visit_c2_arm_1"&R01_lab_results$redcap_event_name!="visit_u24_arm_1")  , ]
# R01_lab_results<- R01_lab_results[, !grepl("u24|sample|pedsql_", names(R01_lab_results) ) ]
# 
# #cohort  
# R01_lab_results$id_cohort<-substr(R01_lab_results$person_id, 2, 2) #F and M are AIC, 0 C and D are other
# R01_lab_results <- within(R01_lab_results, id_cohort[R01_lab_results$id_cohort=="M"] <- "F")
# 
# #Creating a new variable by studyID for study site
# R01_lab_results$id_city<-substr(R01_lab_results$person_id, 1, 1) #C is Chulaimbo, K is Kisumu, M is Msambweni, U is Ukunda, one 0 not sure, R is also Chulaimbo, G stands for Nganja (one of the subparts of Msambweni), L is for Mililani (part of Msambweni)
# 
# R01_lab_results$Site<-NA
# R01_lab_results <- within (R01_lab_results, Site[R01_lab_results$id_city=="C" | R01_lab_results$id_city=="R"] <- "Chulaimbo")
# R01_lab_results <- within (R01_lab_results, Site[R01_lab_results$id_city=="K"] <- "Kisumu")
# R01_lab_results <- within (R01_lab_results, Site[R01_lab_results$id_city=="M" | R01_lab_results$id_city=="G" | R01_lab_results$id_city=="L"] <- "Msambweni")
# R01_lab_results <- within (R01_lab_results, Site[R01_lab_results$id_city=="U" ] <- "Ukunda")
# 
# #Removing the one studyID starting with a O
# R01_lab_results <- R01_lab_results[which(R01_lab_results$id_city!="O"), ]
# 
# # interview dates ---------------------------------------------------------
# R01_lab_results$Date <- ifelse(is.na(R01_lab_results$interview_date), 
#                                    R01_lab_results$interview_date_aic,
#                                    R01_lab_results$interview_date)
# 
# R01_lab_results$Date <- ymd(R01_lab_results$Date)
# R01_lab_results$month_year <- as.yearmon(R01_lab_results$Date)
# R01_lab_results$year <- year(as.Date(R01_lab_results$Date, origin = '1900-1-1'))
# 
# #fever
# R01_lab_results$fever_by_temp <- ifelse(R01_lab_results$temp >=38.0 & R01_lab_results$temp < 43, 1, 0)
# R01_lab_results$Symptoms_all_cohorts <- paste0(R01_lab_results$symptoms_aic, R01_lab_results$symptoms_aic)
# R01_lab_results$Symptoms_all_cohorts<-tolower(R01_lab_results$Symptoms_all_cohorts)
# R01_lab_results$fever_by_symptoms<-grepl("fever", R01_lab_results$Symptoms_all_cohorts)
# R01_lab_results$fever_by_symptoms <- ifelse(R01_lab_results$fever_by_symptoms==TRUE, 1, 0)
# R01_lab_results$fever <- R01_lab_results$fever_by_temp + R01_lab_results$fever_by_symptoms
# R01_lab_results$fever <- ifelse(R01_lab_results$fever>=1,1,0)
# 
# ##age group---------------------------------------------------
# # R01_lab_results$age = R01_lab_results$age_calc_rc  # your new merged column starts with age_calc_rc
# # R01_lab_results$age[!is.na(R01_lab_results$aic_calculated_age)] = R01_lab_results$aic_calculated_age[!is.na(R01_lab_results$aic_calculated_age)]  # merge with aic_calculated_age
# # R01_lab_results$age[!is.na(R01_lab_results$age_calc)] = R01_lab_results$age_calc[!is.na(R01_lab_results$age_calc)]  # merge with age_calc
# # 
# # R01_lab_results$age<-round(R01_lab_results$age)
# # 
# # R01_lab_results$age_group<-NA
# # R01_lab_results <- within(R01_lab_results, age_group[age<=2] <- "under 2")
# # R01_lab_results <- within(R01_lab_results, age_group[age>2 & age<=5] <- "2-5")
# # R01_lab_results <- within(R01_lab_results, age_group[age>5 & age<=10] <- "6-10")
# # R01_lab_results <- within(R01_lab_results, age_group[age>10 & age<=15] <- "11-15")
# # R01_lab_results <- within(R01_lab_results, age_group[age>15] <- "over 15")
# # R01_lab_results$age_group <- factor(R01_lab_results$age_group, levels = c("under 2", "2-5", "6-10", "11-15", "over 15"))
# 
# #pcr and ufi ---------------------------------
# pcr_ufi_results <- R01_lab_results[,c("person_id", "id_cohort", "Site", "redcap_event_name"
#                                       , "Date"
#                                       , "result_pcr_denv_stfd"
#                                       # , "pcr_denv_stanford_complete"
#                                       , "result_pcr_chikv_stfd"
#                                       # , "pcr_chikv_stanford_complete"
#                                       , "result_pcr_denv_kenya"
#                                       # , "pcr_denv_kenya_complete"
#                                       , "result_pcr_chikv_kenya"
#                                       # , "pcr_chikv_kenya_complete"
#                                       , "denv_result_ufi"
#                                       , "chikv_result_ufi"
#                                       , "fever"
#                                       , "date_symptom_onset")]
# 
# pcr_ufi_visits <- subset(pcr_ufi_results, redcap_event_name != "patient_informatio_arm_1")
# visits<-unique(pcr_ufi_visits$redcap_event_name)
# pcr_ufi.df <- pcr_ufi_results[,c("person_id", "id_cohort", "Site")]
# pcr_ufi.df <- unique(pcr_ufi.df)
# 
# for (i in 1:length(visits)){
#   visit <- visits[i]
#   visitName <- substr(visit, 1,7)
#   pcr.ufi.Subset <- subset(pcr_ufi_results, redcap_event_name == visit)
#   pcr.ufi.Subset$denv_pcr_ufi <- rowSums(pcr.ufi.Subset[,c("result_pcr_denv_stfd", "result_pcr_denv_kenya", "denv_result_ufi")], na.rm=TRUE)
#   pcr.ufi.Subset$denv_pcr_ufi[pcr.ufi.Subset$denv_pcr_ufi > 0] <- 1
#   pcr.ufi.Subset$chikv_pcr_ufi <- rowSums(pcr.ufi.Subset[,c("result_pcr_chikv_stfd", "result_pcr_chikv_kenya", "chikv_result_ufi")], na.rm=TRUE)
#   pcr.ufi.Subset$chikv_pcr_ufi[pcr.ufi.Subset$chikv_pcr_ufi > 0] <- 1
#   colnames(pcr.ufi.Subset)[5:ncol(pcr.ufi.Subset)] <- paste(visitName, colnames(pcr.ufi.Subset)[5:ncol(pcr.ufi.Subset)], sep = "_")
#   pcr.ufi.Subset <- pcr.ufi.Subset[,c(1:3,5,12:ncol(pcr.ufi.Subset))]
#   pcr_ufi.df <- merge(pcr_ufi.df, pcr.ufi.Subset, by=c("person_id", "id_cohort", "Site"), all.x=T)
# }
# 
# #IgG ---------------------------------------
# seroconverter<-R01_lab_results[, grepl("person_id|Site|id_cohort|redcap_event|ab_|bc_|cd_|de_|ef_|fg_|gh_", names(R01_lab_results))]
# seroconverter<-seroconverter[, !grepl("malaria|tested|freezer|rack|kenya", names(seroconverter))]
# seroconverter<-seroconverter[which(seroconverter$redcap_event_name=="patient_informatio_arm_1"),]
# seroconverter<-seroconverter[, -which(names(seroconverter) %in% c("redcap_event_name"))]
# 
# #combine pcr, ufi, and igg test results ----
# test_results <- merge(pcr_ufi.df, seroconverter, by=c("person_id", "id_cohort", "Site"), all.x=T)
# 
# disease <- c("denv", "chikv")
# visitNum <- c("visit_a_", "visit_b_", "visit_c_", "visit_d_", "visit_e_", "visit_f_", "visit_g_", "visit_h_")
# 
# for (virus in disease){
#   for (visit in visitNum){
#     column.name <- paste0(visit, "infected_", virus)
#     pcrUfi <- paste0(visit, virus, "_pcr_ufi")
#     test_results[column.name]<-NA
#     visNum <- substr(visit, 7,7)
#     if (visNum == "a"){
#       iggNum <- "ab_"
#     } else if (visNum == "b"){
#       iggNum <- "bc_"
#     } else if (visNum == "c"){
#       iggNum <- "cd_"
#     } else if (visNum == "d"){
#       iggNum <- "de_"
#     } else if (visNum == "e"){
#       iggNum <- "ef_"
#     } else if (visNum == "f"){
#       iggNum <- "fg_"
#     } else if (visNum == "g"){
#       iggNum <- "gh_"
#     }
#     igg <- paste0(iggNum, virus, "_stfd_igg")
#     test_results[column.name][test_results[pcrUfi]==0|test_results[igg]==0]<-0
#     test_results[column.name][test_results[pcrUfi]==1|test_results[igg]==1]<-1
#   }
# }
# 
# #AIC results -------------------------------
# aic.results <- subset(test_results, id_cohort == "F")
# aic.results <- aic.results[,c("person_id", "Site", "visit_a_Date", "visit_a_fever"
#                               , "visit_a_date_symptom_onset", "visit_a_infected_denv"
#                               , "visit_a_infected_chikv")]
# 
# write.csv(aic.results, "Kenya/Concatenated_Data/aic_results.csv", row.names = F)
# 
# # sum(aic.results$visit_a_infected_denv == 1, na.rm=T)
# # sum(aic.results$visit_a_infected_chikv == 1, na.rm=T)
# 
# #aic timeline -----------------------------
# aic.df <- subset(aic.results, !is.na(visit_a_Date))
# aic.df$visit_a_Date <- as.Date(aic.df$visit_a_Date, "%Y-%m-%d")
# aic.df$infected.t0 <- as.Date(NA)
# aic.df$infected.t1 <- as.Date(NA)
# 
# for (j in 1:nrow(aic.df)){
#   if (!is.na(aic.df$visit_a_fever[j]) == TRUE & aic.df$visit_a_date_symptom_onset[j] <= 7){
#     aic.df$infected.t1[j] <- aic.df$visit_a_Date[j] - aic.df$visit_a_date_symptom_onset[j]
#     aic.df$infected.t0[j] <- aic.df$infected.t1[j] - 6
#   } else {
#     aic.df$infected.t1[j] <- aic.df$visit_a_Date[j]
#     aic.df$infected.t0[j] <- aic.df$visit_a_Date[j] - 6
#   }
# }
# 
# write.csv(aic.df, "Kenya/Concatenated_Data/aic_timeline.csv", row.names = F)
# 
# sum(aic.df$visit_a_infected_denv == 1, na.rm=T)
# sum(aic.df$visit_a_infected_chikv == 1, na.rm=T)
# # aicK <- subset(aic, visit_a_infected_chikv_stfd == 1 & Site == "Kisumu")
# # aicU <- subset(aic, visit_a_infected_chikv_stfd == 1 & Site == "Ukunda")
# # aicC <- subset(aic, visit_a_infected_chikv_stfd == 1 & Site == "Chulaimbo")
# # aicM <- subset(aic, visit_a_infected_chikv_stfd == 1 & Site == "Msambweni")
# 
# 
# aic.df <- aic.results[!is.na(aic.results$interview_date_aic),]
# 
# 
# 
# #HCC results -------------------------------
# hcc.results <- subset(test_results, id_cohort == "C")
# hcc.results <- hcc.results[, grepl("person_id|Site|Date|fever|symptom_onset|infected", names(hcc.results))]
# write.csv(hcc.results, "Kenya/Concatenated_Data/hcc_results.csv", row.names = F)
# 
# # sum(hcc.results[,c(27:34)], na.rm=T) # denv positive
# # sum(hcc.results[,c(35:42)], na.rm=T) # chikv positive
