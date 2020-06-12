# Concatenate housing info from Kenya ---------------------------------------
rm(list=ls()) #remove previous variable assignments

# load data
source("Codes/REDCap_import_case_data.R")

# load libraries
library(tidyverse)

# Create a new variable by studyID for study site
R01_lab_results$id_city <- substr(R01_lab_results$person_id, 1, 1) #C is Chulaimbo, K is Kisumu, M is Msambweni, U is Ukunda, one 0 not sure, R is also Chulaimbo, G stands for Nganja (one of the subparts of Msambweni), L is for Mililani (part of Msambweni)
R01_lab_results$Site <- NA
R01_lab_results <- within (R01_lab_results, Site[R01_lab_results$id_city=="C" | R01_lab_results$id_city=="R"] <- "Chulaimbo")
R01_lab_results <- within (R01_lab_results, Site[R01_lab_results$id_city=="K"] <- "Kisumu")
R01_lab_results <- within (R01_lab_results, Site[R01_lab_results$id_city=="M" | R01_lab_results$id_city=="G" | R01_lab_results$id_city=="L"] <- "Msambweni")
R01_lab_results <- within (R01_lab_results, Site[R01_lab_results$id_city=="U" ] <- "Ukunda")

# house water source
table(R01_lab_results$dem_water_source, R01_lab_results$Site)

# drinking water source
table(R01_lab_results$drinking_water_source, R01_lab_results$Site)

personal_water_sources <- R01_lab_results %>%
  filter(!is.na(drinking_water_source)) %>%
  filter(drinking_water_source != 9) %>%
  group_by(Site) %>%
  summarize(# river_pond = sum(drinking_water_source == 1)/length(Site),
            # rain_water = sum(drinking_water_source == 2)/length(Site),
            # public_well_borehole = sum(drinking_water_source == 3)/length(Site),
            # inside_well = sum(drinking_water_source == 4)/length(Site),
            drinking_water_tap_piped = sum(drinking_water_source == 5)/length(Site))
            # water_truck = sum(drinking_water_source == 6)/length(Site))

house_water_sources <- R01_lab_results %>%
  filter(!is.na(dem_water_source)) %>%
  group_by(Site) %>%
  summarize(house_piped_private = sum(dem_water_source == 1)/length(Site),
            house_piped_public = sum(dem_water_source == 2)/length(Site))
# 1, Piped house |2 , Piped public  |  3, Public well |  4, Rain     |  5, River canal | 6, Dam/pond | 7, Borehole | 8, Borehole pump  |    9, Other

roof_types <- R01_lab_results %>%
  filter(!is.na(roof_type)) %>%
  filter(roof_type != 9) %>%
  group_by(Site) %>%
  summarize(roof_natural = sum(roof_type == 1)/length(Site),
            roof_iron = sum(roof_type == 2)/length(Site),
            roof_plastic = sum(roof_type == 3)/length(Site),
            roof_other = sum(roof_type == 4)/length(Site))

floor_types <- R01_lab_results %>%
  filter(!is.na(floor_type)) %>%
  filter(floor_type != 9) %>%
  group_by(Site) %>%
  summarize(floor_dirt = sum(floor_type == 1)/length(Site),
            floor_wood = sum(floor_type == 2)/length(Site),
            floor_cement = sum(floor_type == 3)/length(Site),
            floor_tile = sum(floor_type == 4)/length(Site),
            floor_other = sum(floor_type == 5)/length(Site))

screens <- R01_lab_results %>%
  filter(!is.na(dem_hoh_screens)) %>%
  group_by(Site) %>%
  summarize(screens_yes = sum(dem_hoh_screens == 1)/length(Site),
            screens_some = sum(dem_hoh_screens == 2)/length(Site),
            screens_no = sum(dem_hoh_screens == 0)/length(Site))

bednets <- R01_lab_results %>%
  filter(!is.na(dem_hoh_number_bednet)) %>%
  group_by(Site) %>%
  summarize(bednets = sum(dem_hoh_number_bednet > 0)/length(Site))

bednets[4,] <- c("Msambweni", 0)

# merge data
kenya_house_quality <- personal_water_sources %>%
  left_join(house_water_sources, by = "Site") %>%
  left_join(roof_types, by = "Site") %>%
  left_join(floor_types, by = "Site") %>%
  left_join(screens, by = "Site") %>%
  left_join(bednets, by = "Site")

# subset data to match with ecuador
kenya_house_quality2 <- kenya_house_quality[,c("Site", "house_piped_private", "floor_dirt", "floor_wood", "floor_cement", "floor_other", "screens_yes", "screens_no", "bednets")] 

# ecuador data from Ryan and Lippi unpublished
ecuador_house_quality <- data.frame("Site" = c("Huaquillas", "Machala", "Portovelo", "Zaruma"),
                        "house_piped_private" = c(54/60, 68/75, 75/75, 65/68),
                        "floor_dirt" = c(3/60, 6/75, 0, 1/68),
                        "floor_wood" = c(0, 4/75, 4/75, 1/68),
                        "floor_cement" = c(52/60, 65/75, 71/75, 63/68),
                        "floor_other" = c(0, 0, 0, 3/68),
                        "screens_yes" = c(13/60, 30/75, 7/75, 1/68),
                        "screens_no" = c(4/60, 45/75, 68/75, 67/68),
                        "bednets" = c(46/60, 41/75, 11/75, 10/47)) 

house_quality <- rbind(kenya_house_quality2, ecuador_house_quality)

# save data 
save(house_quality, file = "data/house_quality_info.RData")
