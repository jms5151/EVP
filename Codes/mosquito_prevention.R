# Concatenate mosquito prevention efforts from Kenya ---------------------------------------
rm(list=ls()) #remove previous variable assignments

# load data
source("Codes/REDCap_import_vector_data.R")
source("Codes/REDCap_import_case_data.R")

# load libraries
library(tidyverse)

# vector data
# set site names
redcap_vector$Site <- substr(redcap_vector$unique_house_id, 1, 1)
redcap_vector$Site[redcap_vector$Site == "C"] <- "Chulaimbo"
redcap_vector$Site[redcap_vector$Site == "K"] <- "Kisumu"
redcap_vector$Site[redcap_vector$Site == "M"] <- "Msambweni"
redcap_vector$Site[redcap_vector$Site == "U"] <- "Ukunda"

# number of houses that spray by site
spray <- redcap_vector %>% 
  filter(!is.na(insecticide_sprayed)) %>%
  group_by(Site, unique_house_id) %>%
  summarize(spray = max(insecticide_sprayed, na.rm = T)) %>%
  group_by(Site) %>%
  summarize(percent_spray = sum(spray)/length(Site))

# case data
# set site names
R01_lab_results$id_city <- substr(R01_lab_results$person_id, 1, 1) #C is Chulaimbo, K is Kisumu, M is Msambweni, U is Ukunda, one 0 not sure, R is also Chulaimbo, G stands for Nganja (one of the subparts of Msambweni), L is for Mililani (part of Msambweni)
R01_lab_results$Site <- NA
R01_lab_results <- within (R01_lab_results, Site[R01_lab_results$id_city=="C" | R01_lab_results$id_city=="R"] <- "Chulaimbo")
R01_lab_results <- within (R01_lab_results, Site[R01_lab_results$id_city=="K"] <- "Kisumu")
R01_lab_results <- within (R01_lab_results, Site[R01_lab_results$id_city=="M" | R01_lab_results$id_city=="G" | R01_lab_results$id_city=="L"] <- "Msambweni")
R01_lab_results <- within (R01_lab_results, Site[R01_lab_results$id_city=="U" ] <- "Ukunda")

coil <- R01_lab_results %>%
  filter(!is.na(mosquito_coil)) %>%
  group_by(Site, person_id) %>%
  summarize(coil = max(mosquito_coil, na.rm=T)) %>%
  group_by(Site) %>%
  summarize(percent_coil = sum(coil > 0)/length(Site))

# merge
prevention <- left_join(spray, coil, by="Site")

# ecuador data from Ryan et al. 
ecuador_prevention <- data.frame("Site" = c("Huaquillas", "Machala", "Portovelo", "Zaruma"),
                                 "percent_spray" = c(0.189, 0.275, 0.456, 0.365),
                                 "percent_coil" = rep(NA, 4)) 

# join
prevention <- rbind(prevention, ecuador_prevention)

# save data 
save(prevention, file = "SEI-SEIR_Arboviruses/data/prevention.RData")
