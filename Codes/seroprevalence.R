# seroprevalence --------------------------------------------
# source redcap data 
source("Codes/REDCap_extract_case_data.R")

library(lubridate)


R01_lab_results <- redcap_data

#remove extraneous rows
R01_lab_results<- R01_lab_results[which(!is.na(R01_lab_results$redcap_event_name))  , ]
R01_lab_results<- R01_lab_results[which(R01_lab_results$redcap_event_name!="visit_a2_arm_1"&R01_lab_results$redcap_event_name!="visit_b2_arm_1"&R01_lab_results$redcap_event_name!="visit_c2_arm_1"&R01_lab_results$redcap_event_name!="visit_d2_arm_1"&R01_lab_results$redcap_event_name!="visit_c2_arm_1"&R01_lab_results$redcap_event_name!="visit_u24_arm_1")  , ]
R01_lab_results<- R01_lab_results[, !grepl("u24|sample|pedsql_", names(R01_lab_results) ) ]

#Creating a new variable by studyID for study site
R01_lab_results$id_city<-substr(R01_lab_results$person_id, 1, 1) #C is Chulaimbo, K is Kisumu, M is Msambweni, U is Ukunda, one 0 not sure, R is also Chulaimbo, G stands for Nganja (one of the subparts of Msambweni), L is for Mililani (part of Msambweni)

R01_lab_results$id_site<-NA
R01_lab_results <- within (R01_lab_results, id_site[R01_lab_results$id_city=="C" | R01_lab_results$id_city=="R"] <- "Chulaimbo")
R01_lab_results <- within (R01_lab_results, id_site[R01_lab_results$id_city=="K"] <- "Kisumu")
R01_lab_results <- within (R01_lab_results, id_site[R01_lab_results$id_city=="M" | R01_lab_results$id_city=="G" | R01_lab_results$id_city=="L"] <- "Msambweni")
R01_lab_results <- within (R01_lab_results, id_site[R01_lab_results$id_city=="U" ] <- "Ukunda")

#Removing the one studyID starting with a O
R01_lab_results <- R01_lab_results[which(R01_lab_results$id_city!="O"), ]

# interview dates ---------------------------------------------------------
R01_lab_results$int_date <- ifelse(is.na(R01_lab_results$interview_date),
                                   R01_lab_results$interview_date_aic,
                                   R01_lab_results$interview_date)

R01_lab_results$int_date <- ymd(R01_lab_results$int_date)
R01_lab_results$month_year <- as.yearmon(R01_lab_results$int_date)
R01_lab_results$year <- year(as.Date(R01_lab_results$int_date, origin = '1900-1-1'))

##age group---------------------------------------------------
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

#IgG seroprevalence ----------------------------------------------------------------
seroconverter<-R01_lab_results[, grepl("person_id|id_site|id_cohort|redcap_event|result_igg_chikv_stfd$|result_igg_denv_stfd$|^age$|^age_group$|^year$", names(R01_lab_results))]
seroconverter<-seroconverter[, !grepl("malaria|tested|kenya", names(seroconverter))]

# dengue seroprevalence ------------------------------------------------------------
denv_seroconverter<-seroconverter[!with(seroconverter,is.na(result_igg_denv_stfd)),]

# across years
seroprevalence_denv <- denv_seroconverter %>% group_by(person_id, year) %>% filter(result_igg_denv_stfd == max(result_igg_denv_stfd))
seroprevalence_denv <- seroprevalence_denv[!duplicated(seroprevalence_denv$person_id),]
seroprevalence_denv_Yr <- ddply(seroprevalence_denv, .(year, age_group),
                        summarise,
                        seroprev = sum(result_igg_denv_stfd == 1)/length(result_igg_denv_stfd))

boxplot(seroprev ~ age_group, data=seroprevalence_denv_Yr, col="lightblue", main="Dengue seroprevalence", ylab=c("Seroprevalence"), xlab=c("Age group (years)"))
write.csv(seroprevalence_denv_Yr, "Kenya/Figures/Case_data/Seroprevalence/seroprevalence_denv.csv", row.names = F)

# total seroprevalence for dengue: 0.02924348
seroprevalence_denv_total <- denv_seroconverter %>% group_by(person_id) %>% filter(result_igg_denv_stfd == max(result_igg_denv_stfd))
seroprevalence_denv_total <- seroprevalence_denv_total[!duplicated(seroprevalence_denv_total$person_id),]
seroprev_denv_ttl <- sum(seroprevalence_denv_total$result_igg_denv_stfd == 1)/length(seroprevalence_denv_total$result_igg_denv_stfd)

# chikungunya seroprevalence --------------------------------------------------------
chikv_seroconverter<-seroconverter[!with(seroconverter,is.na(result_igg_chikv_stfd)),]

# across years
seroprevalence_chikv <- chikv_seroconverter %>% group_by(person_id, year) %>% filter(result_igg_chikv_stfd == max(result_igg_chikv_stfd))
seroprevalence_chikv <- seroprevalence_chikv[!duplicated(seroprevalence_chikv$person_id),]
seroprevalence_chikv_Yr <- ddply(seroprevalence_chikv, .(year, age_group),
                                summarise,
                                seroprev = sum(result_igg_chikv_stfd == 1)/length(result_igg_chikv_stfd))

boxplot(seroprev ~ age_group, data=seroprevalence_chikv_Yr, col="lightblue", main="Chikungunya seroprevalence", ylab=c("Seroprevalence"), xlab=c("Age group (years)"))
write.csv(seroprevalence_chikv_Yr, "Kenya/Figures/Case_data/Seroprevalence/seroprevalence_chikv.csv", row.names = F)

# total seroprevalence for dengue: 0.02990937
seroprevalence_chikv_total <- chikv_seroconverter %>% group_by(person_id) %>% filter(result_igg_chikv_stfd == max(result_igg_chikv_stfd))
seroprevalence_chikv_total <- seroprevalence_chikv_total[!duplicated(seroprevalence_chikv_total$person_id),]
seroprev_chikv_ttl <- sum(seroprevalence_chikv_total$result_igg_chikv_stfd == 1)/length(seroprevalence_chikv_total$result_igg_chikv_stfd)
