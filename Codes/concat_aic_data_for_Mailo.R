rm(list=ls()) #remove previous variable assignments

# source redcap data -------------------------------------------------------
source("Codes/REDCap_extract_case_data.R")

# subset aic cohort  
R01_lab_results$id_cohort<-substr(R01_lab_results$person_id, 2, 2) #F and M are AIC, 0 C and D are other
R01_lab_results <- within(R01_lab_results, id_cohort[R01_lab_results$id_cohort=="M"] <- "F")
R01_lab_results <- subset(R01_lab_results, id_cohort == "F")

# Create a new variable by studyID for study site
R01_lab_results$id_city<-substr(R01_lab_results$person_id, 1, 1) #C is Chulaimbo, K is Kisumu, M is Msambweni, U is Ukunda, one 0 not sure, R is also Chulaimbo, G stands for Nganja (one of the subparts of Msambweni), L is for Mililani (part of Msambweni)
R01_lab_results$id_site<-NA
R01_lab_results <- within (R01_lab_results, id_site[R01_lab_results$id_city=="C" | R01_lab_results$id_city=="R"] <- "Chulaimbo")
R01_lab_results <- within (R01_lab_results, id_site[R01_lab_results$id_city=="K"] <- "Kisumu")
R01_lab_results <- within (R01_lab_results, id_site[R01_lab_results$id_city=="M" | R01_lab_results$id_city=="G" | R01_lab_results$id_city=="L"] <- "Msambweni")
R01_lab_results <- within (R01_lab_results, id_site[R01_lab_results$id_city=="U" ] <- "Ukunda")

# Remove the one studyID starting with a O
R01_lab_results <- R01_lab_results[which(R01_lab_results$id_city!="O"), ]

# interview dates ---------------------------------------------------------
R01_lab_results$interview_date_aic <- as.Date(R01_lab_results$interview_date_aic, "%Y-%m-%d")

# fever ---------------------------------------------------
R01_lab_results$temp[R01_lab_results$temp == 385] <- 38.5
R01_lab_results$fever_by_temp <- ifelse(R01_lab_results$temp >=38.0 & R01_lab_results$temp < 43, 1, 0)
R01_lab_results$Symptoms_all_cohorts <- paste0(R01_lab_results$symptoms_aic, R01_lab_results$symptoms_aic)
R01_lab_results$Symptoms_all_cohorts<-tolower(R01_lab_results$Symptoms_all_cohorts)
R01_lab_results$fever_by_symptoms<-grepl("fever", R01_lab_results$Symptoms_all_cohorts)
R01_lab_results$fever_by_symptoms <- ifelse(R01_lab_results$fever_by_symptoms==TRUE, 1, 0)
R01_lab_results$fever <- R01_lab_results$fever_by_temp + R01_lab_results$fever_by_symptoms
R01_lab_results$fever <- ifelse(R01_lab_results$fever>=1,1,0)

# age group ---------------------------------------------------
R01_lab_results$age = R01_lab_results$age_calc_rc  # your new merged column starts with age_calc_rc
R01_lab_results$age[!is.na(R01_lab_results$aic_calculated_age)] = R01_lab_results$aic_calculated_age[!is.na(R01_lab_results$aic_calculated_age)]  # merge with aic_calculated_age
R01_lab_results$age[!is.na(R01_lab_results$age_calc)] = R01_lab_results$age_calc[!is.na(R01_lab_results$age_calc)]  # merge with age_calc

R01_lab_results$age<-round(R01_lab_results$age)

R01_lab_results$age_group<-NA
R01_lab_results <- within(R01_lab_results, age_group[age<=2] <- "under 2")
R01_lab_results <- within(R01_lab_results, age_group[age>2 & age<=5] <- "2-5")
R01_lab_results <- within(R01_lab_results, age_group[age>5 & age<=10] <- "6-10")
R01_lab_results <- within(R01_lab_results, age_group[age>10 & age<=15] <- "11-15")
R01_lab_results <- within(R01_lab_results, age_group[age>15] <- "over 15")
R01_lab_results$age_group <- factor(R01_lab_results$age_group, levels = c("under 2", "2-5", "6-10", "11-15", "over 15"))

aic.tsi.foi <- R01_lab_results[,c("person_id", "id_site", "redcap_event_name", "interview_date_aic"
                                  , "date_symptom_onset", "age", "age_group", "gender_aic"
                                  , "occupation_aic", "village_aic", "child_travel", "where_travel_aic"
                                  , "stay_overnight_aic", "temp", "fever_by_symptoms", "fever"
                                  , "fever_contact", "result_igg_denv_stfd", "result_igm_denv_stfd"
                                  , "result_pcr_denv_kenya", "result_pcr_denv_stfd", "denv_result_ufi"
                                  , "result_igg_chikv_stfd", "result_igm_chikv_stfd"
                                  , "result_pcr_chikv_kenya", "result_pcr_chikv_stfd", "chikv_result_ufi"
                                  , "serotype_pcr_denv_kenya___1", "serotype_pcr_denv_kenya___2"
                                  , "serotype_pcr_denv_kenya___3", "serotype_pcr_denv_kenya___4"
                                  , "serotype_pcr_denv_stfd___1", "serotype_pcr_denv_stfd___2"
                                  , "serotype_pcr_denv_stfd___3", "serotype_pcr_denv_stfd___4"
                                  , "ab_denv_stfd_igg", "bc_denv_stfd_igg"
                                  , "cd_denv_stfd_igg", "de_denv_stfd_igg", "ef_denv_stfd_igg"
                                  , "fg_denv_stfd_igg", "gh_denv_stfd_igg", "ab_chikv_stfd_igg"
                                  , "bc_chikv_stfd_igg", "cd_chikv_stfd_igg", "de_chikv_stfd_igg"
                                  , "ef_chikv_stfd_igg", "fg_chikv_stfd_igg", "gh_chikv_stfd_igg")]

aic.tsi.foi <- aic.tsi.foi[!grepl("2|patient", aic.tsi.foi$redcap_event_name),]
write.csv(aic.tsi.foi, "C:/Users/Jamie/Box Sync/FOI Kenya Project/expanded_visits_aic.csv", row.names = F)


# from Amy
# reshape sereoconverter vars -----------------------------------------------------------------
#seroconversion
seroconverter<-R01_lab_results[, grepl("person_id|redcap_event|ab_|bc_|cd_|de_|ef_|fg_|gh_", names(R01_lab_results))]
seroconverter<-seroconverter[, !grepl("malaria|tested", names(seroconverter))]
seroconverter<-seroconverter[which(seroconverter$redcap_event_name=="patient_informatio_arm_1"),]

#order
seroconverter<-seroconverter[,order(colnames(seroconverter))]

seroconverter<-seroconverter[order(-(grepl('gh_$', names(seroconverter)))+1L)]
seroconverter<-seroconverter[order(-(grepl('fg_$', names(seroconverter)))+1L)]
seroconverter<-seroconverter[order(-(grepl('ef_$', names(seroconverter)))+1L)]
seroconverter<-seroconverter[order(-(grepl('de_$', names(seroconverter)))+1L)]
seroconverter<-seroconverter[order(-(grepl('cd_$', names(seroconverter)))+1L)]
seroconverter<-seroconverter[order(-(grepl('bc_$', names(seroconverter)))+1L)]
seroconverter<-seroconverter[order(-(grepl('ab_$', names(seroconverter)))+1L)]

nameVec <- names(seroconverter)
v.names=c('chikv_kenya_igg', 'chikv_stfd_igg', 'denv_kenya_igg', 'denv_stfd_igg')
times = c("ab_", "bc_", "cd_", "de_", "ef_", "fg_", "gh_")    
seroconverter_long<-reshape(seroconverter, idvar = "person_id", varying = 1:28,  direction = "long", timevar = "visit", times=times, v.names=v.names)

table(seroconverter$ab_chikv_stfd_igg)
table(seroconverter$ab_denv_stfd_igg)
table(seroconverter$bc_chikv_stfd_igg)
table(seroconverter$bc_denv_stfd_igg)
table(seroconverter_long$chikv_stfd_igg, seroconverter_long$visit)
table(seroconverter_long$denv_stfd_igg, seroconverter_long$visit)

seroconverter_long <- within(seroconverter_long, visit[visit=="ab_"] <- "visit_a_arm_1")
seroconverter_long <- within(seroconverter_long, visit[visit=="bc_"] <- "visit_b_arm_1")
seroconverter_long <- within(seroconverter_long, visit[visit=="cd_"] <- "visit_c_arm_1")
seroconverter_long <- within(seroconverter_long, visit[visit=="de_"] <- "visit_d_arm_1")
seroconverter_long <- within(seroconverter_long, visit[visit=="ef_"] <- "visit_e_arm_1")
seroconverter_long <- within(seroconverter_long, visit[visit=="fg_"] <- "visit_f_arm_1")
seroconverter_long <- within(seroconverter_long, visit[visit=="gh_"] <- "visit_g_arm_1")
seroconverter_long$redcap_event_name<-seroconverter_long$visit
names(seroconverter_long)[names(seroconverter_long) == 'denv_kenya_igg'] <- 'seroc_denv_kenya_igg'
names(seroconverter_long)[names(seroconverter_long) == 'chikv_kenya_igg'] <- 'seroc_chikv_kenya_igg'
names(seroconverter_long)[names(seroconverter_long) == 'denv_stfd_igg'] <- 'seroc_denv_stfd_igg'
names(seroconverter_long)[names(seroconverter_long) == 'chikv_stfd_igg'] <- 'seroc_chikv_stfd_igg'

