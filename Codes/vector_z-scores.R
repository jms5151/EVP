#-------- calculate z-scores for vector data
rm(list=ls()) #remove previous variable assignments
source("C:/Users/Jamie/Box Sync/DENV/Codes/REDCap_extractVectorData.R")

library(dplyr)
library(plyr)

#---- read in data if not connected to VPN
house <- read.csv("VectorData/redcap/house.csv", head=T)
larva <- read.csv("VectorData/redcap/larva.csv", head=T)
prokopak <- read.csv("VectorData/redcap/prokopak.csv", head=T)
bgSent <- read.csv("VectorData/redcap/bgSent.csv", head=T)
ovitrap <- read.csv("VectorData/redcap/ovitrap.csv", head=T)
hlc <- read.csv("VectorData/redcap/hlc.csv", head=T)

#---- House
hse <- ddply(house, .(compound_house_id),
             summarise,
             study_site = unique(na.omit(study_site)))

#---- Larvae
lv <- larva[!is.na(larva$redcap_repeat_instrument),]
lv$Date <- as.Date(lv$date_time_larva, "%Y-%m-%d")

lv2 <- ddply(lv, .(compound_house_id, Date),
             summarise,
             early_instars = sum(na.omit(early_instars_larva_1)) + sum(na.omit(early_instars_larva_2)) 
             + sum(na.omit(early_instars_larva_3)) + sum(na.omit(early_instars_larva_4)) 
             + sum(na.omit(early_instars_larva_5)) + sum(na.omit(early_instars_larva_6)) 
             + sum(na.omit(early_instars_larva_7)) + sum(na.omit(early_instars_larva_8)) 
             + sum(na.omit(early_instars_larva_9)) + sum(na.omit(early_instars_larva_10))
             + sum(na.omit(early_instars_larva_11)) + sum(na.omit(early_instars_larva_11))
             + sum(na.omit(early_instars_larva_12)) + sum(na.omit(early_instars_larva_13))
             + sum(na.omit(early_instars_larva_14)) + sum(na.omit(early_instars_larva_15)),
             late_instars = sum(na.omit(late_instars_larva_1)) + sum(na.omit(late_instars_larva_2)) 
             + sum(na.omit(late_instars_larva_3)) + sum(na.omit(late_instars_larva_4)) 
             + sum(na.omit(late_instars_larva_5)) + sum(na.omit(late_instars_larva_6)) 
             + sum(na.omit(late_instars_larva_7)) + sum(na.omit(late_instars_larva_8)) 
             + sum(na.omit(late_instars_larva_9)) + sum(na.omit(late_instars_larva_10))
             + sum(na.omit(late_instars_larva_11)) + sum(na.omit(late_instars_larva_11))
             + sum(na.omit(late_instars_larva_12)) + sum(na.omit(late_instars_larva_13))
             + sum(na.omit(late_instars_larva_14)) + sum(na.omit(late_instars_larva_15)),
             pupae = sum(na.omit(pupae_larva_1)) + sum(na.omit(pupae_larva_2)) 
             + sum(na.omit(pupae_larva_3)) + sum(na.omit(pupae_larva_4)) 
             + sum(na.omit(pupae_larva_5)) + sum(na.omit(pupae_larva_6)) 
             + sum(na.omit(pupae_larva_7)) + sum(na.omit(pupae_larva_8)) 
             + sum(na.omit(pupae_larva_9)) + sum(na.omit(pupae_larva_10))
             + sum(na.omit(pupae_larva_11)) + sum(na.omit(pupae_larva_11))
             + sum(na.omit(pupae_larva_12)) + sum(na.omit(pupae_larva_13))
             + sum(na.omit(pupae_larva_14)) + sum(na.omit(pupae_larva_15)))

lv3 <- merge(lv2, hse, by="compound_house_id") #4087 and 4282 are missing from house  
write.csv(lv3, "Concatenated_Data/vector/larvae.csv", row.names=F)
#---- Prokopak
prok <- prokopak[!is.na(prokopak$redcap_repeat_instrument),]
prok$Date <- as.Date(prok$date_time_prokopack, "%Y-%m-%d")

prok2 <- ddply(prok, .(compound_house_id, Date),
               summarise,
               a.aegypti_male = sum(na.omit(aedes_agypti_male_prokopack_indoor))+sum(na.omit(aedes_agypti_male_prokopack_outdoor)),
               a.aegypti_unfed = sum(na.omit(aedes_agypti_unfed_prokopack_indoor))+sum(na.omit(aedes_agypti_unfed_prokopack_outdoor)),
               a.aegypti_bloodfed = sum(na.omit(aedes_agypti_blood_fed_prokopack_indoor))+sum(na.omit(aedes_agypti_bloodfed_prokopack_outdoor)),
               a.aegypti_halfgravid = sum(na.omit(aedes_agypti_half_gravid_prokopack_indoor))+sum(na.omit(aedes_agypti_half_gravid_prokopack_outdoor)),
               a.aegypti_gravid = sum(na.omit(aedes_agypti_gravid_prokopack_indoor))+sum(na.omit(aedes_agypti_gravid_prokopack_outdoor)),
               a.aegypti_female = (a.aegypti_unfed + a.aegypti_bloodfed + a.aegypti_halfgravid + a.aegypti_gravid),
               a.aegypti_total = (a.aegypti_male + a.aegypti_female),
               a.aegypti_frac_female = a.aegypti_female/a.aegypti_total)

prok3 <- merge(prok2, hse, by="compound_house_id") # missing houses
write.csv(prok3, "Concatenated_Data/vector/prokopak.csv", row.names=F)
#---- BG Sentinel Traps
bg <- bgSent[!is.na(bgSent$survey_bg),]
bg$Date <- as.Date(bg$date_bg, "%Y-%m-%d")

bg2 <- ddply(bg, .(compound_house_id, survey_bg),
             summarise,
             dropoffDate = min(Date),
             pickupDate = max(Date),
             a.aegypti_male = sum(na.omit(aedes_agypti_male_bg)),
             a.aegypti_unfed = sum(na.omit(aedes_agypti_unfed_bg)),
             a.aegypti_bloodfed = sum(na.omit(aedes_agypti_bloodfed_bg)),
             a.aegypti_halfgravid = sum(na.omit(aedes_agypti_half_gravid_bg)),
             a.aegypti_gravid = sum(na.omit(aedes_agypti_gravid_bg)),
             a.aegypti_female = (a.aegypti_unfed + a.aegypti_bloodfed + a.aegypti_halfgravid + a.aegypti_gravid),
             a.aegypti_total = (a.aegypti_male + a.aegypti_female),
             a.aegypti_frac_female = a.aegypti_female/a.aegypti_total)

# bg2[5:11] <- apply(bg2[5:11], 2, function (x) (x-mean(x))/sd(x))
bg3 <- merge(bg2, hse, by="compound_house_id") # missing houses
# y <- bg2[,c(5:11)]
# ts.plot(y,gpars= list(col=rainbow(10)))
write.csv(bg3, "Concatenated_Data/vector/bg.csv", row.names=F)

#---- Ovitrap
ovi <- ovitrap[!is.na(ovitrap$redcap_repeat_instrument),]
ovi$date_set <- as.Date(ovi$date_set_day_ovitrap, "%Y-%m-%d")
ovi$date_collected <- as.Date(ovi$date_collected_day_ovitrap, "%Y-%m-%d")

ovi2 <- ddply(ovi, .(compound_house_id, redcap_repeat_instance),
              summarise,
              dropoffDate = min(date_set),
              pickupDate = max(date_collected),
              eggs = sum(na.omit(egg_count_ovitrap_in)) + sum(na.omit(egg_count_ovitrap_out)))

ovi2$num.days <- difftime(ovi2$pickupDate, ovi2$dropoffDate, units="days")

ovi3 <- merge(ovi2, hse, by="compound_house_id")
write.csv(ovi3, "Concatenated_Data/vector/ovitrap.csv", row.names=F)

#---- Human Landing Catches (HLC)
hlc2 <- hlc[!is.na(hlc$redcap_repeat_instrument),]
hlc2$Date <- as.Date(hlc2$date_hlc, "%Y-%m-%d")

hlc3 <- ddply(hlc2, .(compound_house_id, survey_hlc),
              summarise,
              start_date = min(Date),
              end_date = max(Date),
              num.days = difftime(end_date, start_date, units="days"),
              a.aegypti_male = sum(na.omit(aedes_agypti_male_hlc)),
              a.aegypti_unfed = sum(na.omit(aedes_agypti_unfed_hlc)),
              a.aegypti_bloodfed = sum(na.omit(aedes_agypti_bloodfed_hlc)),
              a.aegypti_halfgravid = sum(na.omit(aedes_agypti_half_gravid_hlc)),
              a.aegypti_gravid = sum(na.omit(aedes_agypti_gravid_hlc)),
              a.aegypti_female = (a.aegypti_unfed + a.aegypti_bloodfed + a.aegypti_halfgravid + a.aegypti_gravid),
              a.aegypti_total = (a.aegypti_male + a.aegypti_female),
              a.aegypti_frac_female = a.aegypti_female/a.aegypti_total)

hlc4 <- merge(hlc3, hse, by="compound_house_id") # missing houses
hlc5 <- subset(hlc3, num.days <5) # while there are issues
write.csv(hlc5, "Concatenated_Data/vector/hlc.csv", row.names=F)

