# vector data -----------------------------------------------------------------
source("Codes/REDCap_extract_climate_and_vector_data.R")

library(dplyr)
library(data.table)
library(stringr)

# house ids ------
redcap_clim_vec$vector_house_id <- substr(redcap_clim_vec$redcap_event_name, 1,4)
redcap_clim_vec$vector_house_id <- as.numeric(redcap_clim_vec$vector_house_id)

HVS <- redcap_clim_vec[,c("vector_house_id", "village_estate", "study_site")]
HVS$filledRows <- rowSums(!is.na(HVS))
HVS2 <- HVS %>%
  group_by(vector_house_id) %>%
  slice(which.max(filledRows))
HVS2 <- HVS2[complete.cases(HVS2$vector_house_id), 1:ncol(HVS2)-1]

# occupants|domestic_animals are not filled in as of March-14-2018, add if filled in later
house.df <- redcap_clim_vec[, grepl("date|vector_house_id|study_site|house|rooms|insecticide|coil|bed_net|eaves", names(redcap_clim_vec))]

# concatenate house_wall and house roof into single columns -----
wallTypes <- which(names(house.df)%in%c("house_wall___1", "house_wall___2", "house_wall___3", "house_wall_other"))
house.df$house_wall <- ifelse(is.na(house.df$house_wall___1), NA, names(house.df[,c(wallTypes)])[which(house.df[,c(wallTypes)] == 1, arr.ind=T)[, "col"]])

roofTypes <- which(names(house.df)%in%c("house_roof___1", "house_room___2", "house_roof___3", "house_roof___4", "house_roof___98", "house_roof___99", "house_roof_other"))
house.df$house_roof <- ifelse(is.na(house.df$house_roof___1), NA, names(house.df[,c(roofTypes)])[which(house.df[,c(roofTypes)] == 1, arr.ind=T)[, "col"]])

house.df2 <- house.df[, grepl("date|vector_house_id|^house_wall$|^house_roof$|rooms|insecticide|coil|bed_net|eaves", names(house.df))]
house.df2$filledRows <- rowSums(!is.na(house.df2))
house.df3 <- house.df2 %>%
  group_by(vector_house_id, date_collected) %>%
  slice(which.max(filledRows))
house.info <- house.df3[!is.na(house.df3$vector_house_id),]
house.info <- house.info[, !grepl("ovitrap|filledRows", names(house.info))]

house.final <- merge(house.df3, HVS2, by="vector_house_id", all.x=T)
house.final <- house.final[, !grepl("ovitrap|filledRows", names(house.final))]

# genus and species codes from redcap -----
genus_larva<-c(1:5,97:99)
genus<-c("Aedes", "Anopheles", "Culex", "Toxorhynchites","Mansonia", "unknown", "none", "other")
genus <- data.frame(genus_larva, genus)

aedes_species_larva<-c(1:13,98:99)
aedes_species<-c("Ae. aegypti", "Ae. simpsoni", "Ae. africanus", "Ae. vittatus", "Ae. metallicus", "Ae. (Stegomyia)spp.", "Ae. circumluteolus", "Ae. mcintoshi", "Ae. tricholabis", "Ae. ochraceus", "Ae. sudanensis", "Ae. cumminsi", "Ae. dentatus", "other", "refused")
Aespecies <- data.frame(aedes_species_larva, aedes_species)

anopheles_species_larva<-c(1:3,6,8,9,98:99)
anopheles_species <- c("An. arabiensis", "An. funestus", "Anopheles gambiae", "An. pharoensis", "An. coustani", "An. squamosus", "other", "refused")
Anspecies <- data.frame(anopheles_species_larva, anopheles_species)

# larvae -----------------------------------------------
larvae <- subset(redcap_clim_vec, redcap_repeat_instrument == "larva")
larvae.df <- larvae[, grepl("instars|pupae|date|redcap|larva|vector", names(larvae) ) ]

# concatenate team lead into single column -----
larvae.leaders <- which(names(larvae.df)%in%c("team_leader_larva___1", "team_leader_larva___2", "team_leader_larva___3", "team_leader_larva___4", "team_leader_larva___98", "team_leader_larva___99", "team_leader_other_larva"))

larvae.df$team_lead <- ifelse(is.na(larvae.df$team_leader_larva___1), 0
                              , names(larvae.df[,c(larvae.leaders)])[which(larvae.df[,c(larvae.leaders)] == 1
                                                                           , arr.ind=T)[, "col"]])

# separate and stack indoor and outdoor data
larvae.in <- larvae.df[, grepl("^team_lead$|date|redcap|_1_in|vector|notes|complete", names(larvae.df) ) ]
larvae.in$location <- "indoors"
colnames(larvae.in) <- gsub("_1_in", "", colnames(larvae.in))

larvae.out <- larvae.df[, grepl("^team_lead$|date|redcap|_1_out|vector|notes|complete", names(larvae.df) ) ]
larvae.out$location <- "outdoors"
colnames(larvae.out) <- gsub("_1_out", "", colnames(larvae.out))

larvae.long <- rbind(larvae.in, larvae.out)

# concatenate genus into one column
larvae.long <- merge(larvae.long, genus, by="genus_larva", all.x = T)

# concatenate species into one column
larvae.long <- merge(larvae.long, Aespecies, by="aedes_species_larva", all.x = T)
larvae.long <- merge(larvae.long, Anspecies, by="anopheles_species_larva", all.x = T)

larvae.long$species <- paste(larvae.long$aedes_species, larvae.long$anopheles_species, sep=" ")
larvae.long$species <- gsub(" NA|NA ", "", larvae.long$species)

larvae.final <- larvae.long[,c("vector_house_id", "date_collected", "redcap_repeat_instance"
                               , "early_instars_larva", "late_instars_larva", "pupae_larva"
                               , "genus", "species", "location", "roomplace_larva"
                               , "habitat_type_other_larva", "habitat_size_larva"
                               , "habitat_type_larva", "team_lead", "larva_complete"
                               , "redcap_repeat_instrument")]

larvae.fn <- merge(larvae.final, HVS2, by = "vector_house_id", all.x=T)
larvae.fn <- merge(larvae.fn, house.info, by = c("vector_house_id", "date_collected"), all.x=T)
write.csv(larvae.fn, "Kenya/Concatenated_Data/vector/redcap_larvae.csv", row.names=F)

# prokopack  -----------------------------------------------
prokopack <- subset(redcap_clim_vec, redcap_repeat_instrument == "prokopack")
prokopack.df <- prokopack[, grepl("date|redcap|prokopack|vector", names(prokopack) ) ]

# concatenate team lead into single column -----
prok.leaders <- which(names(prokopack.df)%in%c("team_leader_prokopack___1", "team_leader_prokopack___2", "team_leader_prokopack___3", "team_leader_prokopack___4", "team_leader_prokopack___98", "team_leader_prokopack___99", "team_leader_prokopack_other"))
prokopack.df$team_lead <- ifelse(is.na(prokopack.df$team_leader_prokopack___1), 0, names(prokopack.df[,c(prok.leaders)])[which(prokopack.df[,c(prok.leaders)] == 1, arr.ind=T)[, "col"]])

# separate and stack indoor and outdoor data
prokopack.in <- subset(prokopack.df, indoors_prokopack___1 == 1)
prokopack.in <- prokopack.in[, grepl("^team_lead$|date|time|house|redcap|_indoor|vector|notes|complete", names(prokopack.in) ) ]
prokopack.in$location <- "indoors"
colnames(prokopack.in) <- gsub("_indoor", "", colnames(prokopack.in))
setnames(prokopack.in, old=c("aedes_agypti_blood_fed_prokopack", "aedes_simpsoni_blood_fed_prokopack"
                             , "ga_half_gravid_prokopack", "fumale_prokopack", "fu_half_gravid_prokopack"
                             , "cuunfed_prokopack"),
         new=c("aedes_agypti_bloodfed_prokopack", "aedes_simpsoni_bloodfed_prokopack"
               , "ga_halfgravid_prokopack", "fu_male_prokopack", "fu_halfgravid_prokopack"
               , "cu_unfed_prokopack"))

prokopack.out <- subset(prokopack.df, indoors_prokopack___2 == 1)
prokopack.out <- prokopack.out[, grepl("^team_lead$|date|time|house|redcap|_outdoor|vector|notes|complete", names(prokopack.out) ) ]
prokopack.out$location <- "outdoors"
colnames(prokopack.out) <- gsub("_outdoor", "", colnames(prokopack.out))
setnames(prokopack.out, old=c("gabloodfed_prokopack", "gahalfgravid_prokopack", "fugravid_prokopack"),
         new=c("ga_bloodfed_prokopack", "ga_halfgravid_prokopack", "fu_gravid_prokopack"))

# reshape from wide to long format -----
prokopack.sub <- rbind(prokopack.in, prokopack.out)
prokopack.sub2 <- prokopack.sub[, !grepl("^date_collected_complete$|event|ovitrap", names(prokopack.sub) ) ]
colnames(prokopack.sub2) <- gsub("_prokopack$", "", colnames(prokopack.sub2))

prokopack.long <- prokopack.sub2[0,c(1:7,38:41)]
prokopack.long$mosquitoes <- as.integer()
prokopack.long$status <- as.character()

for (i in 8:37){
  temp.pr.sub <- prokopack.sub2[,c(1:7,38:41,i)]
  name <- colnames(temp.pr.sub)[ncol(temp.pr.sub)]
  colnames(temp.pr.sub)[ncol(temp.pr.sub)] <- "mosquitoes"
  temp.pr.sub$status <- name
  prokopack.long <- rbind(prokopack.long, temp.pr.sub)
}

# separate species and status
prokopack.long$species <-  gsub("aedes_spp.*", "aedes_spp", prokopack.long$status)
prokopack.long$species <-  gsub("aedes_aegypti.*|aedes_agypti.*", "aedes_aegypti", prokopack.long$species)
prokopack.long$species <-  gsub("aedes_simpsoni.*", "aedes_simpsoni", prokopack.long$species)
prokopack.long$species <-  gsub("ga.*", "an_gambie", prokopack.long$species)
prokopack.long$species <-  gsub("fu.*", "an_funestus", prokopack.long$species)
prokopack.long$species <-  gsub("cu.*", "culex", prokopack.long$species)
prokopack.long$species <-  gsub("toxo.*", "toxo", prokopack.long$species)

prokopack.long$sex <-  gsub(".*male.*", "male", prokopack.long$status)
prokopack.long$sex <-  gsub(".*unfed.*|.*bloodfed.*|.*blood_fed.*|.*halfgravid.*|.*half_gravid.*|.*gravid.*", "female", prokopack.long$sex)

prokopack.long$fed <-  gsub(".*male.*", "male", prokopack.long$status)
prokopack.long$fed <-  gsub(".*unfed.*", "unfed", prokopack.long$fed)
prokopack.long$fed <-  gsub(".*bloodfed.*|.*blood_fed.*", "bloodfed", prokopack.long$fed)
prokopack.long$fed <-  gsub(".*halfgravid.*|.*half_gravid.*", "halfgravid", prokopack.long$fed)
prokopack.long$fed <-  gsub(".*gravid.*", "gravid", prokopack.long$fed)

prokopack.final <- prokopack.long[, !grepl("status", names(prokopack.long) ) ]

prokopack.fn <- merge(prokopack.final, HVS2, by = "vector_house_id", all.x=T)
prokopack.fn <- merge(prokopack.fn, house.info, by = c("vector_house_id", "date_collected"), all.x=T)
write.csv(prokopack.fn, "Kenya/Concatenated_Data/vector/redcap_prokopack.csv", row.names=F)

# hlc -----------------------------------------------
hlc <- subset(redcap_clim_vec, redcap_repeat_instrument == "hlc")
hlc.df <- hlc[, grepl("date|30|redcap|hlc|vector", names(hlc) ) ]

# concatenate team lead into single column -----
hlc.leaders <- which(names(hlc.df)%in%c("team_leader_hlc___1", "team_leader_hlc___2", "team_leader_hlc___3", "team_leader_hlc___4", "team_leader_hlc___98", "team_leader_hlc_other"))
hlc.df$team_lead <- ifelse(is.na(hlc.df$team_leader_hlc___1), 0, names(hlc.df[,c(hlc.leaders)])[which(hlc.df[,c(hlc.leaders)] == 1, arr.ind=T)[, "col"]])

# concatenate team lead into single column -----
hlc.df$collector <- ifelse(is.na(hlc.df$collector_name_hlc___1), 0, names(hlc.df[,c(17:87)])[which(hlc.df[,c(17:87)] == 1, arr.ind=T)[, "col"]])

hlc.sub <- hlc.df[, !grepl("^team_leader_hlc|event|collector_name_hlc|^date_collected_complete$|^collection_place_hlc_other$|ovitrap", names(hlc.df) ) ]

# make dataframe with time information
hlc.t <- hlc.sub[, !grepl("gravid|fed|male", names(hlc.sub) ) ]

# reshape from wide to long format -----
hlc.t.long <- melt(hlc.t, id.vars = c("vector_house_id", "date_collected", "day_hlc", "collection_place_hlc"
                                      , "team_lead", "collector", "hlc_complete", "redcap_repeat_instrument"
                                      , "redcap_repeat_instance", "survey_hlc"), variable.name = "spp_time"
                   , value.name = "mosquitoes")

# separate species and time
hlc.t.long$species <- (str_extract(hlc.t.long$spp_time, "[aA-zZ]+"))
hlc.t.long$species <- gsub("_$", "", hlc.t.long$species)
hlc.t.long$time <- gsub("[^0-9\\.]", "", hlc.t.long$spp_time) 

hlc.t.final <- hlc.t.long[, !grepl("spp_time", names(hlc.t.long) ) ]

hlc.t.fn <- merge(hlc.t.final, HVS2, by = "vector_house_id", all.x=T)
hlc.t.fn <- merge(hlc.t.fn, house.info, by = c("vector_house_id", "date_collected"), all.x=T)
write.csv(hlc.t.fn, "Kenya/Concatenated_Data/vector/redcap_hlc_time.csv", row.names=F)

# make dataframe with sex information
hlc.s <- hlc.sub[, grepl("date|redcap|survey|day|collection|complete|vector|team|collector|gravid|fed|male", names(hlc.sub) ) ]

# reshape from wide to long format -----
hlc.s.long <- melt(hlc.s, id.vars = c("vector_house_id", "date_collected", "day_hlc", "collection_place_hlc"
                                      , "team_lead", "collector", "hlc_complete", "redcap_repeat_instrument"
                                      , "redcap_repeat_instance", "survey_hlc"), variable.name = "status"
                                      , value.name = "mosquitoes")

# separate species and status
hlc.s.long$species <-  gsub("aedes_spp.*", "aedes_spp", hlc.s.long$status)
hlc.s.long$species <-  gsub("aedes_aegypti.*|aedes_agypti.*", "aedes_aegypti", hlc.s.long$species)
hlc.s.long$species <-  gsub("aedes_simpsoni.*", "aedes_simpsoni", hlc.s.long$species)
hlc.s.long$species <-  gsub("ga.*", "an_gambie", hlc.s.long$species)
hlc.s.long$species <-  gsub("fu.*", "an_funestus", hlc.s.long$species)
hlc.s.long$species <-  gsub("cu.*", "culex", hlc.s.long$species)

hlc.s.long$sex_status <-  gsub(".*male.*", "male", hlc.s.long$status)
hlc.s.long$sex_status <-  gsub(".*unfed.*", "unfed", hlc.s.long$sex_status)
hlc.s.long$sex_status <-  gsub(".*bloodfed.*|.*blood_fed.*", "bloodfed", hlc.s.long$sex_status)
hlc.s.long$sex_status <-  gsub(".*halfgravid.*|.*half_gravid.*", "halfgravid", hlc.s.long$sex_status)
hlc.s.long$sex_status <-  gsub(".*gravid.*", "gravid", hlc.s.long$sex_status)

hlc.s.final <- hlc.s.long[, !grepl("^status$", names(hlc.s.long) ) ]
hlc.s.fn <- merge(hlc.s.final, HVS2, by = "vector_house_id", all.x=T)
hlc.s.fn <- merge(hlc.s.fn, house.info, by = c("vector_house_id", "date_collected"), all.x=T)
write.csv(hlc.s.fn, "Kenya/Concatenated_Data/vector/redcap_hlc_sex.csv", row.names=F)

# ovitrap -----------------------------------------------
ovitrap <- subset(redcap_clim_vec, redcap_repeat_instrument == "ovitrap")
ovitrap.df <- ovitrap[, grepl("date|redcap|ovitrap|vector", names(ovitrap) ) ]

# concatenate team lead into single column -----
ovi.leaders <- which(names(ovitrap.df)%in%c("team_leader_ovitrap___1", "team_leader_ovitrap___2", "team_leader_ovitrap___3", "team_leader_ovitrap___4", "team_leader_ovitrap___98", "team_leader_ovitrap___99", "team_leader_ovitrap_other"))
ovitrap.df$team_lead <- ifelse(is.na(ovitrap.df$team_leader_ovitrap___1), 0, names(ovitrap.df[,c(ovi.leaders)])[which(ovitrap.df[,c(ovi.leaders)] == 1, arr.ind=T)[, "col"]])

# merge room/place as microhabitat
room_ovitrap_in <- c(1:7,98)
place_in <- c("Bedroom", "Bathroom", "Bedroom", "Kitchen", "Sitting Room", "Store", "Varander", "other")
rooms <- data.frame(room_ovitrap_in, place_in)
ovitrap.df <- merge(ovitrap.df, rooms, by="room_ovitrap_in", all.x = T)

place_ovitrap_out <- c(1:10,98)
place_out <- c("Banana", "Bathroom", "Flower bush", "Fence", "Garden", "Bush", "Sitting Room", "Sugarcane", "Tall grass", "Wall", "other")
places <- data.frame(place_ovitrap_out, place_out, by="place_ovitrap_out")
ovitrap.df <- merge(ovitrap.df, places, by="place_ovitrap_out", all.x = T)

# separate and stack indoor and outdoor data
ovi.in <- ovitrap.df[, grepl("^team_lead$|date|redcap|_in|vector|notes|complete", names(ovitrap.df) ) ]
ovi.in$location <- "indoors"
colnames(ovi.in) <- gsub("_in$", "", colnames(ovi.in))

ovi.out <- ovitrap.df[, grepl("^team_lead$|date|redcap|_out|vector|notes|complete", names(ovitrap.df) ) ]
ovi.out$location <- "outdoors"
colnames(ovi.out) <- gsub("_out", "", colnames(ovi.out))

ovi.df <- rbind.fill(ovi.in, ovi.out)

# concatenate genus into one column
colnames(genus)[1]<-"genus_ovitrap"
ovi.df <- merge(ovi.df, genus, by="genus_ovitrap", all.x = T)

# concatenate species into one column
colnames(Aespecies)[1]<-"aedes_species_ovitrap"
ovi.df <- merge(ovi.df, Aespecies, by="aedes_species_ovitrap", all.x = T)

colnames(Anspecies)[1]<-"anopheles_species_ovitrap"
ovi.df <- merge(ovi.df, Anspecies, by="anopheles_species_ovitrap", all.x = T)

ovi.df$species <- apply(ovi.df[,c("aedes_species", "anopheles_species")], 1, function(x) toString(na.omit(x)))
# there are mixed species so right now keep all mixed with aedes aegypti as aedes aegypti (2/15/18)
ovi.df$species <- gsub("Ae. aegypti.*", "Ae. aegypti", ovi.df$species)
ovi.df$species <- gsub("other.*", "other", ovi.df$species)

ovi.final <- ovi.df[, c("vector_house_id", "date_set_day_ovitrap", "date_collected"
                        , "redcap_repeat_instance", "team_lead", "place", "location", "egg_count_ovitrap"
                        , "genus", "species", "ovitrap_complete", "redcap_repeat_instrument")]

ovi.fn <- merge(ovi.final, HVS2, by = "vector_house_id", all.x=T)
ovi.fn <- merge(ovi.fn, house.info, by = c("vector_house_id", "date_collected"), all.x=T)
write.csv(ovi.fn, "Kenya/Concatenated_Data/vector/redcap_ovitrap.csv", row.names=F)

# bg -----------------------------------------------
bg <- subset(redcap_clim_vec, redcap_repeat_instrument == "bg")
bg.df <- bg[, grepl("date|redcap|bg|vector", names(bg) ) ]

# concatenate team lead into single column -----
bg.leaders <- which(names(bg.df)%in%c("team_leader_bg___1", "team_leader_bg___2", "team_leader_bg___3", "team_leader_bg___4", "team_leader_bg___98", "team_leader_bg___99"))
bg.df$team_lead <- ifelse(is.na(bg.df$team_leader_bg___1), 0, names(bg.df[,c(bg.leaders)])[which(bg.df[,c(bg.leaders)] == 1, arr.ind=T)[, "col"]])

# reshape from wide to long format -----
bg.sub <- bg.df[, !grepl("^team_leader_bg|ovitrap", names(bg.df) ) ]
colnames(bg.sub) <- gsub("_bg$", "", colnames(bg.sub))

bg.long <- bg.sub[0,c(1:10,46:49)]
bg.long$mosquitoes <- as.integer()
bg.long$status <- as.character()

for (i in 11:45){
  temp.bg.sub <- bg.sub[,c(1:10,46:49,i)]
  name <- colnames(temp.bg.sub)[ncol(temp.bg.sub)]
  colnames(temp.bg.sub)[ncol(temp.bg.sub)] <- "mosquitoes"
  temp.bg.sub$status <- name
  bg.long <- rbind(bg.long, temp.bg.sub)
}

# separate species and status
bg.long$species <-  gsub("aedes_spp.*", "aedes_spp", bg.long$status)
bg.long$species <-  gsub("aedes_aegypti.*|aedes_agypti.*", "aedes_aegypti", bg.long$species)
bg.long$species <-  gsub("aedes_simpsoni.*", "aedes_simpsoni", bg.long$species)
bg.long$species <-  gsub("ga.*", "an_gambie", bg.long$species)
bg.long$species <-  gsub("fu.*", "an_funestus", bg.long$species)
bg.long$species <-  gsub("cu.*", "culex", bg.long$species)
bg.long$species <-  gsub("toxo.*", "toxo", bg.long$species)

bg.long$sex <-  gsub(".*male.*", "male", bg.long$status)
bg.long$sex <-  gsub(".*unfed.*|.*bloodfed.*|.*blood_fed.*|.*halfgravid.*|.*half_gravid.*|.*gravid.*", "female", bg.long$sex)

bg.long$fed <-  gsub(".*male.*", "male", bg.long$status)
bg.long$fed <-  gsub(".*unfed.*", "unfed", bg.long$fed)
bg.long$fed <-  gsub(".*bloodfed.*|.*blood_fed.*", "bloodfed", bg.long$fed)
bg.long$fed <-  gsub(".*halfgravid.*|.*half_gravid.*", "halfgravid", bg.long$fed)
bg.long$fed <-  gsub(".*gravid.*", "gravid", bg.long$fed)

bg.final <- bg.long[, !grepl("status", names(bg.long) ) ]

bg.fn <- merge(bg.final, HVS2, by = "vector_house_id", all.x=T)
bg.fn <- merge(bg.fn, house.info, by = c("vector_house_id", "date_collected"), all.x=T)
write.csv(bg.fn, "Kenya/Concatenated_Data/vector/redcap_bg.csv", row.names=F)
