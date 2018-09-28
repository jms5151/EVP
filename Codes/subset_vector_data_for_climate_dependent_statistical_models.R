# subset vector data for climate dependent statistical models ---------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(plyr)
library(car)
library(MASS)
library(ggplot2)

# load data ----------------------------------------------------------------------------
# source("Codes/summarize_and_plot_vector_survey_frequency.R")
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
  mozDF[,"date_collected"] <- as.Date(mozDF[,"date_collected"], "%Y-%m-%d")
  mozDF$mn.yr <- format(mozDF$date_collected, "%Y-%m")
  mozDF[,"study_site"] <- as.factor(mozDF[,"study_site"])
  mozDF[,"vector_house_id"] <- as.factor(mozDF[,"vector_house_id"])
  mozDF$house <- paste(mozDF$study_site, mozDF$vector_house_id, sep='-')
  levels(mozDF$study_site) <- factor(c("1","2","3","4"), labels=c("Chulaimbo","Kisumu","Msambweni", "Ukunda")) 
  assign(dfNames[i], mozDF)
}

# strict subset (does not allow for any missing data) ---------------------------------
# skip this section for broad subset
ovi.freq <- as.data.frame(cbind(table(ovitrap$mn.yr, ovitrap$study_site)))
ovi.freq$min <- apply(ovi.freq, 1, FUN=min)
train.ovi <- subset(ovitrap, mn.yr >= "2014-06" & mn.yr <= "2015-12")
test.ovi <- subset(ovitrap, mn.yr >= "2016-05" & mn.yr <= "2016-12")
ovitrap <- rbind(train.ovi, test.ovi)

lar.freq <- as.data.frame(cbind(table(larvae$mn.yr, larvae$study_site)))
lar.freq$min <- apply(lar.freq, 1, FUN=min)
train.lar <- subset(larvae, mn.yr >= "2014-05" & mn.yr <= "2016-12")
test.lar <- subset(larvae, mn.yr >= "2017-08" & mn.yr <= "2018-02")
larvae <- rbind(train.lar, test.lar)

prok.freq <- as.data.frame(cbind(table(prokopack$mn.yr, prokopack$study_site)))
prok.freq$min <- apply(prok.freq, 1, FUN=min)
train.prok <- subset(prokopack, mn.yr >= "2014-06" & mn.yr <= "2016-08")
test.prok <- subset(prokopack, mn.yr >= "2017-09" & mn.yr <= "2018-02")
prokopack <- rbind(train.prok, test.prok)

# minRow <- min(which(sitefreq$min>0))
# endRow <- min(which(sitefreq[c(minRow:nrow(sitefreq)), "min"]==0))-2+minRow
# train.sub <- sitefreq[c(minRow:(minRow+endRow)),]

# -------------------------------------------------------------------------------------
# start here for broad subset (allows for some missing data) 
trap_frequency <- read.csv("Kenya/Concatenated_Data/vector/summary_visits/all_traps_house_visit_summary.csv", head = T)

# identify houses without site information
missing.site.num <- subset(trap_frequency, study_site == "Missing_site_info")
missing.site.num <- missing.site.num$vector_house_id 

# identify houses with fewer than 5 visits
few.site.visits <- subset(trap_frequency, numVisits < 5)  
few.site.visits <- few.site.visits$vector_house_id

# create list of houses with no site information and/or fewer than 5 visits
remove.houses <- c(missing.site.num, few.site.visits)
remove.houses <- unique(remove.houses)

# remove houses from each dataset with not enough information
larvae.complete.houses <- larvae[ !larvae$vector_house_id %in% remove.houses,]
ovitrap.complete.houses <- ovitrap[ !ovitrap$vector_house_id %in% remove.houses,]
prokopack.complete.houses <- prokopack[ !prokopack$vector_house_id %in% remove.houses,]

# create house dataset with totals for each mn-yr combination -----------------------
egg_house_totals <- ddply(ovitrap.complete.houses, .(mn.yr, date_collected, study_site, vector_house_id, house), summarise, egg_total = sum(egg_count_ovitrap, na.rm=T))
larvae_house_totals <- ddply(larvae.complete.houses, .(mn.yr, date_collected, study_site, vector_house_id, house), summarise, early_instar_total = sum(early_instars_larva, na.rm=T), late_instar_total = sum(late_instars_larva, na.rm=T), pupae_total = sum(pupae_larva, na.rm=T))
adult_house_totals <- subset(prokopack.complete.houses, species == "aedes_aegypti")
adult_house_totals <- ddply(prokopack.complete.houses, .(mn.yr, date_collected, study_site, vector_house_id, house), summarise, aedes_total = sum(mosquitoes, na.rm=T))

# egg_house_totals <- ddply(ovitrap.complete.houses, .(mn.yr, date_collected, study_site, vector_house_id, house, species), summarise, egg_total = sum(egg_count_ovitrap, na.rm=T))
# larvae_house_totals <- ddply(larvae.complete.houses, .(mn.yr, date_collected, study_site, vector_house_id, house, species), summarise, early_instar_total = sum(early_instars_larva, na.rm=T), late_instar_total = sum(late_instars_larva, na.rm=T), pupae_total = sum(pupae_larva, na.rm=T))
# adult_house_totals <- ddply(prokopack.complete.houses, .(mn.yr, date_collected, study_site, vector_house_id, house, species), summarise, aedes_total = sum(mosquitoes, na.rm=T))
# egg_house_totals <- subset(egg_house_totals, species == "Ae. aegypti")
# larvae_house_totals <- subset(larvae_house_totals, species == "Ae. aegypti")
# adult_house_totals <- subset(adult_house_totals, species == "aedes_aegypti")

# this is just precautionary if houses were used for trapping twice in same month 
egg_house_totals <- ddply(egg_house_totals, .(mn.yr, study_site, vector_house_id, house), summarise, date_collected = max(date_collected), egg_total = sum(egg_total, na.rm=T))
# larvae_house_totals <- ddply(larvae_house_totals, .(mn.yr, study_site, vector_house_id, house), summarise, early_instar_total = sum(early_instar_total, na.rm=T), late_instar_total = sum(late_instar_total, na.rm=T), pupae_total = sum(pupae_total, na.rm=T))
# adult_house_totals <- ddply(adult_house_totals, .(mn.yr, study_site, vector_house_id, house), summarise, aedes_total = sum(aedes_total, na.rm=T))

# create site dataset with totals for each mn-yr combination ------------------------
egg_site_totals <- ddply(egg_house_totals, .(mn.yr, study_site), summarise, date_collected = max(date_collected), mean_eggs = mean(egg_total)/length(vector_house_id), prop.pos = sum(egg_total > 0)/length(egg_total), egg_total = sum(egg_total, na.rm=T), num_houses = length(vector_house_id))
larvae_site_totals <- ddply(larvae_house_totals, .(mn.yr, study_site), summarise, date_collected = max(date_collected), mean_early_instars = mean(early_instar_total)/length(vector_house_id), prop_pos_early_instars = sum(early_instar_total > 0)/length(early_instar_total), early_instar_total = sum(early_instar_total), mean_late_instars = mean(late_instar_total)/length(vector_house_id), prop_pos_late_instars = sum(late_instar_total > 0)/length(late_instar_total), late_instar_total = sum(late_instar_total), mean_pupae = mean(pupae_total)/length(vector_house_id), prop_pos_pupae = sum(pupae_total > 0)/length(pupae_total), pupae_total = sum(pupae_total), num_houses = length(vector_house_id))
adult_site_totals <- ddply(adult_house_totals, .(mn.yr, study_site), summarise, date_collected = max(date_collected), mean_aedes = mean(aedes_total)/length(vector_house_id), prop.pos = sum(aedes_total > 0)/length(aedes_total), aedes_total = sum(aedes_total, na.rm=T), num_houses = length(vector_house_id))

# write data to csv ----------------------------------------------------------------
write.csv(egg_house_totals, "Kenya/Concatenated_Data/vector/house_vector_totals_eggs.csv", row.names=F)
write.csv(larvae_house_totals, "Kenya/Concatenated_Data/vector/house_vector_totals_larvae.csv", row.names=F)
write.csv(adult_house_totals, "Kenya/Concatenated_Data/vector/house_vector_totals_adults.csv", row.names=F)

write.csv(egg_site_totals, "Kenya/Concatenated_Data/vector/site_vector_totals_eggs.csv", row.names=F)
write.csv(larvae_site_totals, "Kenya/Concatenated_Data/vector/site_vector_totals_larvae.csv", row.names=F)
write.csv(adult_site_totals, "Kenya/Concatenated_Data/vector/site_vector_totals_adults.csv", row.names=F)

# # plot house visit frequency being used in models ----------------------------------
# trapDF.house <- list(egg_house_totals, larvae_house_totals, larvae_house_totals, larvae_house_totals, adult_house_totals)
# traps <- c("egg_total", "early_instar_total", "late_instar_total", "pupae_total", "aedes_total")
# 
# theme_set(theme_bw())
# 
# for (i in 1:length(trapDF.house)){
#   df <- trapDF.house[[i]]
#   ggplot() + geom_point(data = df, aes(x = mn.yr, y = house, size=df[,traps[i]], color = study_site)) +
#     theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = traps[i]) +
#     labs(x="", y="house number", size=traps[i], col="Study site")
#   filename=paste0("Kenya/Figures/vectors/survey_frequency/stats_model_data/stats_model_house_",traps[i], ".tiff")
#   ggsave(filename, width = 12.5, height = 10)
# }
# 
# # plot site visit frequency being used in models ----------------------------------
# trapDF.site <- list(egg_site_totals, larvae_site_totals, larvae_site_totals, larvae_site_totals, adult_site_totals)
# traps2 <- c("mean_eggs", "mean_early_instars", "mean_late_instars", "mean_pupae", "mean_aedes")
# 
# for (i in 1:length(trapDF.site)){
#   df <- trapDF.site[[i]]
#   ggplot() + geom_point(data = df, aes(x = mn.yr, y = study_site, size=df[,traps2[i]], color = study_site)) +
#     theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = traps2[i]) +
#     labs(x="", y="house number", size=traps2[i], col="Study site")
#   filename=paste0("Kenya/Figures/vectors/survey_frequency/stats_model_data/stats_model_site_",traps2[i], ".tiff")
#   ggsave(filename, width = 8, height = 4)
# }