# Concatenate vector data from Kenya field sites ---------------------------------------
# load data
source("Codes/REDCap_import_vector_data.R")

# load libraries
library(plyr)
library(data.table)
library(stringr)

# format dates
date_columns <- grep("date", names(redcap_vector), value = TRUE)
redcap_vector[,date_columns] <- lapply(redcap_vector[,date_columns], as.Date, "%Y-%m-%d")
redcap_vector$Year.Month <- gsub("_arm_1", "", redcap_vector$redcap_event_name)
redcap_vector$Year.Month <- gsub("january", "01", redcap_vector$Year.Month)
redcap_vector$Year.Month <- gsub("february", "02", redcap_vector$Year.Month)
redcap_vector$Year.Month <- gsub("march", "03", redcap_vector$Year.Month)
redcap_vector$Year.Month <- gsub("april", "04", redcap_vector$Year.Month)
redcap_vector$Year.Month <- gsub("may", "05", redcap_vector$Year.Month)
redcap_vector$Year.Month <- gsub("june", "06", redcap_vector$Year.Month)
redcap_vector$Year.Month <- gsub("july", "07", redcap_vector$Year.Month)
redcap_vector$Year.Month <- gsub("august", "08", redcap_vector$Year.Month)
redcap_vector$Year.Month <- gsub("september", "09", redcap_vector$Year.Month)
redcap_vector$Year.Month <- gsub("october", "10", redcap_vector$Year.Month)
redcap_vector$Year.Month <- gsub("november", "11", redcap_vector$Year.Month)
redcap_vector$Year.Month <- gsub("december", "12", redcap_vector$Year.Month)
redcap_vector$Year.Month <- paste0(substr(redcap_vector$Year.Month, 4, 7), "-", substr(redcap_vector$Year.Month, 1,2))

# set site names
redcap_vector$Site <- substr(redcap_vector$unique_house_id, 1, 1)
redcap_vector$Site[redcap_vector$Site == "C"] <- "Chulaimbo"
redcap_vector$Site[redcap_vector$Site == "K"] <- "Kisumu"
redcap_vector$Site[redcap_vector$Site == "M"] <- "Msambweni"
redcap_vector$Site[redcap_vector$Site == "U"] <- "Ukunda"

# eggs  --------------------------------------------------
# subset data for ovitrap surveys
ovitrap <- subset(redcap_vector, redcap_repeat_instrument == "ovitrap")
ovitrap <- ovitrap[, grepl("Site|unique_house_id|ovitrap|redcap|Year.Month", names(ovitrap) ) ]

# subset to rows with aedes aegypti
ovitrap <- subset(ovitrap, aedes_species_ovitrap_in == 1 | aedes_species_ovitrap_out == 1)

# summarize aedes abundances by survey
ovitrap <- ddply(ovitrap, .(Site, Year.Month)
            , summarize
            , egg_total = (sum(egg_count_ovitrap_in, na.rm=T) + sum(egg_count_ovitrap_out, na.rm=T))/length(unique_house_id)
            , numhouses = length(unique_house_id)
            , Date = max(date_ovitrap))

# save data
write.csv(ovitrap, "Concatenated_Data/vector_data/Kenya_ovitrap.csv", row.names = F)

# larvae, pupae, instars --------------------------------
# subset data for larvae surveys
larvae <- subset(redcap_vector, redcap_repeat_instrument == "larva")
larvae <- larvae[, grepl("Site|unique_house_id|larva|redcap|Year.Month", names(larvae) ) ]

# subset to rows with aedes aegypti
# larvae <- subset(larvae, aedes_species_larva_1_in == 1 | aedes_species_larva_1_out == 1)

# summarize aedes abundances by survey
larvae <- ddply(larvae, .(Site, Year.Month), summarize 
                , pupae_total = (sum(pupae_larva_1_in, na.rm=T) + sum(pupae_larva_1_out, na.rm=T))/length(unique_house_id)
                , early_instar_total = (sum(early_instars_larva_1_in, na.rm=T) + sum(early_instars_larva_1_out, na.rm=T))/length(unique_house_id)
                , late_instar_total = (sum(late_instars_larva_1_in, na.rm=T) + sum(late_instars_larva_1_out, na.rm=T))/length(unique_house_id)
                , Date = max(date_larva))

# save data
write.csv(larvae, "Concatenated_Data/vector_data/Kenya_larvae.csv", row.names = F)

# adults - BG ------------------------------------------
# subset data for BG surveys
bg <- subset(redcap_vector, redcap_repeat_instrument == "bg")
bg <- bg[, grepl("Site|unique_house_id|bg|redcap|Year.Month", names(bg) ) ]

# summarize aedes abundances by survey
bg <- ddply(bg, .(Site, Year.Month)
                , summarize
                , aedes_total = (sum(aedes_agypti_male_bg, na.rm=T) + sum(aedes_agypti_unfed_bg, na.rm=T) + sum(aedes_agypti_bloodfed_bg, na.rm=T) + sum(aedes_agypti_half_gravid_bg, na.rm=T) + sum(aedes_agypti_gravid_bg, na.rm=T) + sum(aedes_spp_male_bg, na.rm=T) + sum(aedes_spp_unfed_bg, na.rm=T) + sum(aedes_spp_bloodfed_bg, na.rm=T) + sum(aedes_spp_half_gravid_bg, na.rm=T) + sum(aedes_spp_gravid_bg, na.rm=T))/length(unique_house_id)
                , Date = max(date_bg))

# save data
write.csv(bg, "Concatenated_Data/vector_data/Kenya_bg.csv", row.names = F)

# adults - Prokopack ----------------------------------
# subset data for Prokopack surveys
prokopack <- subset(redcap_vector, redcap_repeat_instrument == "prokopack")
prokopack <- prokopack[, grepl("Site|unique_house_id|prokopack|redcap|Year.Month", names(prokopack) ) ]

# summarize aedes abundances by survey; almost no variation in the number of houses through time
prokopack <- ddply(prokopack, .(Site, Year.Month)
            , summarize
            , aedes_total = sum(aedes_agypti_male_prokopack_indoor, na.rm=T) + 
              sum(aedes_agypti_unfed_prokopack_indoor, na.rm=T) + 
              sum(aedes_agypti_blood_fed_prokopack_indoor, na.rm=T) + 
              sum(aedes_agypti_half_gravid_prokopack_indoor, na.rm=T) + 
              sum(aedes_agypti_gravid_prokopack_indoor, na.rm=T) +
              sum(aedes_agypti_male_prokopack_outdoor, na.rm=T) + 
              sum(aedes_agypti_unfed_prokopack_outdoor, na.rm=T) + 
              sum(aedes_agypti_bloodfed_prokopack_outdoor, na.rm=T) + 
              sum(aedes_agypti_half_gravid_prokopack_outdoor, na.rm=T) + 
              sum(aedes_agypti_gravid_prokopack_outdoor, na.rm=T)
            , Date = max(date_prokopack))

# save data
write.csv(prokopack, "Concatenated_Data/vector_data/Kenya_prokopack.csv", row.names = F)

library(ggplot2)
ggplot() + geom_point(data = bg, aes(x = Year.Month, y = Site))
