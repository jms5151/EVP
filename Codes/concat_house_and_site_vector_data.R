# concat vector data at site and house levels -------------------------------------------
# subset vector data for climate dependent statistical models ---------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(plyr)
library(car)
library(MASS)
library(ggplot2)

# load data ----------------------------------------------------------------------------
# source("Codes/concat_vector_data_from_redcap.R")
# bg <- read.csv("Kenya/Concatenated_Data/vector/redcap_bg.csv", head=T)
# hlc_sex <- read.csv("Kenya/Concatenated_Data/vector/redcap_hlc_sex.csv", head=T)
# hlc_time <- read.csv("Kenya/Concatenated_Data/vector/redcap_hlc_time.csv", head=T)
larvae <- read.csv("Kenya/Concatenated_Data/vector/redcap_larvae.csv", head=T)
ovitrap <- read.csv("Kenya/Concatenated_Data/vector/redcap_ovitrap.csv", head=T)
prokopack <- read.csv("Kenya/Concatenated_Data/vector/redcap_prokopack.csv", head=T)

# classify dates, set month-yr, and make house and study site factors
mozzieTraps <- list(larvae, ovitrap, prokopack)
dfNames <- c("larvae", "ovitrap", "prokopack")
for (i in 1:length(mozzieTraps)){
  mozDF <- mozzieTraps[[i]]
  mozDF$Date <- as.Date(mozDF[,"date_collected"], "%Y-%m-%d")
  mozDF$Year.Month <- format(mozDF$Date, "%Y-%m")
  mozDF$Site <- as.factor(mozDF[,"study_site"])
  mozDF[,"vector_house_id"] <- as.factor(mozDF[,"vector_house_id"])
  mozDF$house <- paste(mozDF$study_site, mozDF$vector_house_id, sep='-')
  levels(mozDF$Site) <- factor(c("1","2","3","4"), labels=c("Chulaimbo","Kisumu","Msambweni", "Ukunda")) 
  assign(dfNames[i], mozDF)
}

# if I want to remove houses with little data ----------------------------
# trap_frequency <- read.csv("Kenya/Concatenated_Data/vector/summary_visits/all_traps_house_visit_summary.csv", head = T)
# 
# # identify houses without site information
# missing.site.num <- subset(trap_frequency, study_site == "Missing_site_info")
# missing.site.num <- missing.site.num$vector_house_id 
# 
# # identify houses with fewer than 5 visits
# few.site.visits <- subset(trap_frequency, numVisits < 5)  
# few.site.visits <- few.site.visits$vector_house_id
# 
# # create list of houses with no site information and/or fewer than 5 visits
# remove.houses <- c(missing.site.num, few.site.visits)
# remove.houses <- unique(remove.houses)
# 
# # remove houses from each dataset with not enough information
# larvae.complete.houses <- larvae[ !larvae$vector_house_id %in% remove.houses,]
# ovitrap.complete.houses <- ovitrap[ !ovitrap$vector_house_id %in% remove.houses,]
# prokopack.complete.houses <- prokopack[ !prokopack$vector_house_id %in% remove.houses,]

# create house dataset with totals for each mn-yr combination -----------------------
# use name.complete.housees if removing houses with few visits
egg_house_totals <- ddply(ovitrap, .(Year.Month, Date, Site, vector_house_id, house, species), summarise, egg_total = sum(egg_count_ovitrap, na.rm=T))
larvae_house_totals <- ddply(larvae, .(Year.Month, Date, Site, vector_house_id, house, species), summarise, early_instar_total = sum(early_instars_larva, na.rm=T), late_instar_total = sum(late_instars_larva, na.rm=T), pupae_total = sum(pupae_larva, na.rm=T))
adult_house_totals <- ddply(prokopack, .(Year.Month, Date, Site, vector_house_id, house, species), summarise, aedes_total = sum(mosquitoes, na.rm=T))

# subset for Aedes aegypti data ------------------------------------------------------------
egg_house_totals_ae <- subset(egg_house_totals, species == "Ae. aegypti" & !is.na(Site))
larvae_house_totals_ae <- subset(larvae_house_totals, species == "Ae. aegypti" & !is.na(Site))
adult_house_totals_ae <- subset(adult_house_totals, species == "aedes_aegypti" & !is.na(Site))

# this is just precautionary if houses were used for trapping twice in same month 
egg_house_totals_ae <- ddply(egg_house_totals_ae, .(Year.Month, Site, vector_house_id, house), summarise, Date = max(Date), egg_total = sum(egg_total, na.rm=T))
# larvae_house_totals <- ddply(larvae_house_totals, .(Year.Month, Site, vector_house_id, house), summarise, early_instar_total = sum(early_instar_total, na.rm=T), late_instar_total = sum(late_instar_total, na.rm=T), pupae_total = sum(pupae_total, na.rm=T))
# adult_house_totals <- ddply(adult_house_totals, .(Year.Month, Site, vector_house_id, house), summarise, aedes_total = sum(aedes_total, na.rm=T))

# write data to csv ----------------------------------------------------------------
write.csv(egg_house_totals_ae, "Kenya/Concatenated_Data/vector/house_vector_totals_eggs.csv", row.names=F)
write.csv(larvae_house_totals_ae, "Kenya/Concatenated_Data/vector/house_vector_totals_larvae.csv", row.names=F)
write.csv(adult_house_totals_ae, "Kenya/Concatenated_Data/vector/house_vector_totals_adults.csv", row.names=F)

# create site dataset with totals for each mn-yr combination ------------------------
egg_site_totals <- ddply(egg_house_totals_ae, .(Year.Month, Site), summarise, Date = max(Date), egg_total = sum(egg_total, na.rm=T), num_houses = length(vector_house_id))
larvae_site_totals <- ddply(larvae_house_totals_ae, .(Year.Month, Site), summarise, Date = max(Date), early_instar_total = sum(early_instar_total), late_instar_total = sum(late_instar_total), pupae_total = sum(pupae_total), num_houses = length(vector_house_id))
adult_site_totals <- ddply(adult_house_totals_ae, .(Year.Month, Site), summarise, Date = max(Date), aedes_total = sum(aedes_total, na.rm=T), num_houses = length(vector_house_id))

# write data to csv ----------------------------------------------------------------
write.csv(egg_site_totals, "Kenya/Concatenated_Data/vector/site_vector_totals_eggs.csv", row.names=F)
write.csv(larvae_site_totals, "Kenya/Concatenated_Data/vector/site_vector_totals_larvae.csv", row.names=F)
write.csv(adult_site_totals, "Kenya/Concatenated_Data/vector/site_vector_totals_adults.csv", row.names=F)
