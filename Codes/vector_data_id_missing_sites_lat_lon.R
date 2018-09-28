# vector data -----------------------------------------------------------------
source("Codes/REDCap_extract_climate_and_vector_data.R")

library(plyr)
library(dplyr)

# house ids ------
climatevars <- c("chulaimbo_hospital_arm_1", "chulaimbo_village_arm_1", "hkki_arm_1", "hkmo_arm_1", "kisumu_estate_arm_1", "msambweni_arm_1", "obama_arm_1", "ukunda_arm_1")
redcap_clim_vec <- redcap_clim_vec[!redcap_clim_vec$redcap_event_name %in% climatevars,] # remove rows for climate data
redcap_clim_vec$vector_house_id <- gsub("_arm_1", "", redcap_clim_vec$redcap_event_name)

# survey frequency per house ------
vec <- redcap_clim_vec[,c("redcap_repeat_instrument", "study_site", "vector_house_id", "latitude", "longitude", "date_collected")]
vec.instr <- subset(vec, redcap_repeat_instrument != "")
vec.instr <- vec.instr[!duplicated(vec.instr), ]
vec.instr <- vec.instr[duplicated(vec.instr$vector_house_id), ]

vec.traps <- ddply(vec.instr, .(vector_house_id),
         summarise,
         Ovitrap_surveys = sum(redcap_repeat_instrument == "ovitrap"),
         Larvae_surveys = sum(redcap_repeat_instrument == "larva"),
         BG_surveys = sum(redcap_repeat_instrument == "bg"), 
         HLC_surveys = sum(redcap_repeat_instrument == "hlc"),
         Prokopack_surveys = sum(redcap_repeat_instrument == "prokopack"))

# study site and lat/lon coordinates per house -----
vec.lat.lon <- subset(vec, redcap_repeat_instrument == "")
vec.lat.lon <- subset(vec.lat.lon, !is.na(study_site))
vec.lat.lon$filled <- ifelse(is.na(vec.lat.lon$latitude), 0, 1)
vec.lat.lon2 <- vec.lat.lon %>%
  group_by(vector_house_id) %>%
  slice(which.max(filled))
vec.lat.lon2 <- as.data.frame(vec.lat.lon2[,c("study_site", "vector_house_id", "latitude", "longitude")])

# combine survey frequency, site, and lat/lon coordinates per house to id missing info -----
vectors_houses <- merge(vec.traps, vec.lat.lon2, by = "vector_house_id", all=TRUE)

# write to csv
write.csv(vectors_houses, "Kenya/Concatenated_Data/vector/summary_visits/vector_houses_missing_sites_latlon.csv", row.names = F)