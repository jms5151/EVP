# determine days in which AIC kids are exposed and infectious --------------------------
rm(list=ls()) #remove previous variable assignments

# load data (header = T just specifies that the data has a header row)
expanded_aic <- read.csv("C:/Users/Jamie/Box Sync/FOI Kenya Project/aic_results_for_mailo_all_visits.csv", header = T, stringsAsFactors = F)

# convert interview date to date class 
expanded_aic$interview_date_aic <- as.Date(expanded_aic$interview_date_aic, "%Y-%m-%d") #"%m/%d/%Y"

# all aic kids are febrile so if days with symptoms is na replace with zero as they had fever on day of visit 
expanded_aic$date_symptom_onset[is.na(expanded_aic$date_symptom_onset)] <- 0

# if kids had fever for more than 7 days treat as day 0
expanded_aic$date_symptom_onset[expanded_aic$date_symptom_onset>7] <- 0

# subset out any 'NA' dates
expanded_aic <- subset(expanded_aic, !is.na(interview_date_aic))

# exposure and infection start and end dates
expanded_aic$date_start_infectious <- (expanded_aic$interview_date_aic - expanded_aic$date_symptom_onset)
expanded_aic$date_end_infectious <- expanded_aic$date_start_infectious + 4
expanded_aic$date_start_exposed <- expanded_aic$date_start_infectious - 7
expanded_aic$date_end_exposed <- expanded_aic$date_start_infectious - 1

# infectious period
mindate_infectious <- min(expanded_aic$date_start_infectious)
maxdate_infectious <- max(expanded_aic$date_end_infectious)

# exposed period
mindate_exposed <- min(expanded_aic$date_start_exposed)
maxdate_exposed <- max(expanded_aic$date_end_exposed)

# create date vectors
inf.dates <- seq.Date(from = mindate_infectious, to = maxdate_infectious, "days")
exp.dates <- seq.Date(from = mindate_exposed, to = maxdate_exposed, "days")

# turn sequential dates into data frames
inf.dates <- as.data.frame(inf.dates)
exp.dates <- as.data.frame(exp.dates)

# add seroconversion for igg at last visit (none yet)
expanded_aic$hi_denv_stfd_igg <- 0
expanded_aic$hi_chikv_stfd_igg <- 0

# create dataframe for infectious period for all kids
peopIDs <- unique(expanded_aic$person_id)
visitNames <- c("a", "b", "c", "d", "e", "f", "g", "h", "i")

for(i in 1:length(peopIDs)){
  visits <- subset(expanded_aic, person_id == peopIDs[i])
  visits[is.na(visits)] <- 0
  exposed <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(exposed) <- c("exp.dates", "denv", "chikv")
  infectious <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(infectious) <- c("inf.dates", "denv", "chikv")
  for (k in 1:nrow(visits)){
    vis <- substr(visits$redcap_event_name[k], 7,7)
    d.seroconv <- paste0(vis, visitNames[which(visitNames==vis)+1], "_denv_stfd_igg")
    visits$denv[k] <- ifelse(visits[k,d.seroconv]==1|visits$result_pcr_denv_kenya[k]==1|visits$result_pcr_denv_stfd[k]==1|visits$denv_result_ufi[k]==1, 1, 0)
    c.seroconv <- paste0(vis, visitNames[which(visitNames==vis)+1], "_chikv_stfd_igg")
    visits$chikv[k] <- ifelse(visits[k,c.seroconv]==1|visits$result_pcr_chikv_kenya[k]==1|visits$result_pcr_chikv_stfd[k]==1|visits$chikv_result_ufi[k]==1, 1, 0)
    # exposed
    tempExp <- as.data.frame(seq.Date(from=visits$date_start_exposed[k], to=visits$date_end_exposed[k], by="days"))
    colnames(tempExp) <- "exp.dates"
    tempExp$denv <- visits$denv[k]
    tempExp$chikv <- visits$chikv[k]
    exposed <- rbind(exposed, tempExp)
    # infectious
    tempInf <- as.data.frame(seq.Date(from=visits$date_start_infectious[k], to=visits$date_end_infectious[k], by="days"))
    colnames(tempInf) <- "inf.dates"
    tempInf$denv <- visits$denv[k]
    tempInf$chikv <- visits$chikv[k]
    infectious <- rbind(infectious, tempInf)
  }
  colnames(exposed)[2] <- paste0("DENV_", visits$id_site[k], "_", as.character(peopIDs[i]))
  colnames(exposed)[3] <- paste0("CHIKV_", visits$id_site[k], "_", as.character(peopIDs[i]))
  exp.dates <- merge(exp.dates, exposed, by = "exp.dates", all.x=T)
  colnames(infectious)[2] <- paste0("DENV_", visits$id_site[k], "_", as.character(peopIDs[i]))
  colnames(infectious)[3] <- paste0("CHIKV_", visits$id_site[k], "_", as.character(peopIDs[i]))
  inf.dates <- merge(inf.dates, infectious, by = "inf.dates", all.x=T)
}
# 
write.csv(exp.dates, "C:/Users/Jamie/Box Sync/FOI Kenya Project/exposed_period.csv", row.names = F)
write.csv(inf.dates, "C:/Users/Jamie/Box Sync/FOI Kenya Project/infectious_period.csv", row.names = F)

