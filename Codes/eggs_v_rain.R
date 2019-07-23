# calculate cumulative rain while set out and cumulative rain in week prior to setting up?
# Concatenate vector data from Kenya field sites ---------------------------------------
# load data
source("Codes/REDCap_import_vector_data.R")

# load libraries
library(data.table)
library(stringr)
library(dplyr)

# set site names
redcap_vector$Site <- substr(redcap_vector$unique_house_id, 1, 1)
redcap_vector$Site[redcap_vector$Site == "C"] <- "Chulaimbo"
redcap_vector$Site[redcap_vector$Site == "K"] <- "Kisumu"
redcap_vector$Site[redcap_vector$Site == "M"] <- "Msambweni"
redcap_vector$Site[redcap_vector$Site == "U"] <- "Ukunda"

# subset data for ovitrap surveys
ovitrap <- subset(redcap_vector, redcap_repeat_instrument == "ovitrap")

# subset to rows with aedes aegypti
ovitrap <- subset(ovitrap, aedes_species_ovitrap_in == 1 | aedes_species_ovitrap_out == 1)

# subset columns
ovitrap <- ovitrap[, grepl("Site|unique_house_id|date_set_day_ovitrap|date_ovitrap|egg_count_ovitrap_in|egg_count_ovitrap_out", names(ovitrap) ) ]

# calculate days out
ovitrap$days_out <- difftime(ovitrap$date_ovitrap, ovitrap$date_set_day_ovitrap)

# fix dates
ovitrap$date_set_day_ovitrap <- if_else(ovitrap$days_out == 34, ovitrap$date_set_day_ovitrap + 30, ovitrap$date_set_day_ovitrap)
ovitrap$date_set_day_ovitrap <- if_else(is.na(ovitrap$date_set_day_ovitrap), ovitrap$date_ovitrap - mean(ovitrap$days_out, na.rm=T), ovitrap$date_set_day_ovitrap)

# update days out
ovitrap$days_out <- difftime(ovitrap$date_ovitrap, ovitrap$date_set_day_ovitrap)

# connect with rain data
climateDat <- read.csv("Concatenated_Data/climate_data/gapfilled_climate_data_Kenya.csv", head=T, stringsAsFactors=F)

for (i in 1:nrow(ovitrap)){
  rainColName <- paste0("GF_", ovitrap$Site[i], "_rain")
  dateStartIndex <- which(climateDat$Date==ovitrap$date_set_day_ovitrap[i])
  dateEndIndex <- which(climateDat$Date==ovitrap$date_ovitrap[i])
  ovitrap$days_out_cum_rain[i] <- sum(climateDat[dateStartIndex:dateEndIndex, rainColName])
  ovitrap$days_out_num_rain_days[i] <- sum(climateDat[dateStartIndex:dateEndIndex, rainColName] > 1)
  # ovitrap$prior_cum_rain[i] <- climateDat[which(climateDat$Date==ovitrap$date_set_day_ovitrap[i]), paste0("GF_", ovitrap$Site[i], "_cumRain")]
  ovitrap$prior_num_rain_days[i] <- sum(climateDat[(dateStartIndex-6):dateStartIndex, rainColName] > 1)
  ovitrap$prior_num_rain_days_month[i] <- climateDat[which(climateDat$Date==ovitrap$date_set_day_ovitrap[i]), paste0("GF_", ovitrap$Site[i], "_rainyDays")]
  ovitrap$prior_cum_rain_1_wk[i] <- climateDat[which(climateDat$Date==ovitrap$date_set_day_ovitrap[i]), paste0("GF_", ovitrap$Site[i], "_cumRain")]
  ovitrap$prior_cum_rain_2_wk[i] <- sum(climateDat[(dateStartIndex-13):dateStartIndex, rainColName])
  ovitrap$prior_cum_rain_3_wk[i] <- sum(climateDat[(dateStartIndex-20):dateStartIndex, rainColName])
  ovitrap$prior_cum_rain_4_wk[i] <- sum(climateDat[(dateStartIndex-27):dateStartIndex, rainColName])
}

ovitrap$prop_rainy_days <- ovitrap$days_out_num_rain_days/as.numeric(ovitrap$days_out)
ovitrap$mean_rain_per_day <- ovitrap$days_out_cum_rain/as.numeric(ovitrap$days_out)

# plot relationships
plot(ovitrap$days_out_cum_rain, ovitrap$egg_count_ovitrap_out, pch=16)
plot(ovitrap$days_out_num_rain_days, ovitrap$egg_count_ovitrap_out, pch=16)
plot(ovitrap$prior_num_rain_days_month, ovitrap$egg_count_ovitrap_out, pch=16)
plot(ovitrap$prior_num_rain_days, ovitrap$egg_count_ovitrap_out, pch=16)
plot(ovitrap$prior_cum_rain_1_wk, ovitrap$egg_count_ovitrap_out, pch=16)
plot(ovitrap$prior_cum_rain_2_wk, ovitrap$egg_count_ovitrap_out, pch=16)
plot(ovitrap$prior_cum_rain_3_wk, ovitrap$egg_count_ovitrap_out, pch=16)
plot(ovitrap$prior_cum_rain_4_wk, ovitrap$egg_count_ovitrap_out, pch=16)

boxplot(ovitrap$egg_count_ovitrap_out~round(ovitrap$mean_rain_per_day), ylim=c(0,200))
plot(ovitrap$prop_rainy_days, ovitrap$egg_count_ovitrap_out, pch=16)
plot(ovitrap$mean_rain_per_day, ovitrap$egg_count_ovitrap_out, pch=16)




# prokopack <- subset(redcap_vector, redcap_repeat_instrument == "prokopack")
# prokopack <- prokopack[, grepl("Site|unique_house_id|prokopack|redcap|Year.Month", names(prokopack) ) ]
# 
# # summarize aedes abundances by survey; almost no variation in the number of houses through time
# prokopack$total <- prokopack$aedes_agypti_male_prokopack_indoor +
#                        prokopack$aedes_agypti_unfed_prokopack_indoor +
#                        prokopack$aedes_agypti_blood_fed_prokopack_indoor +
#                        prokopack$aedes_agypti_half_gravid_prokopack_indoor +
#                        prokopack$aedes_agypti_gravid_prokopack_indoor +
#                        prokopack$aedes_agypti_male_prokopack_outdoor +
#                        prokopack$aedes_agypti_unfed_prokopack_outdoor +
#                        prokopack$aedes_agypti_bloodfed_prokopack_outdoor +
#                        prokopack$aedes_agypti_half_gravid_prokopack_outdoor +
#                        prokopack$aedes_agypti_gravid_prokopack_outdoor
# 
# 
# for (i in 1:nrow(prokopack)){
#   rainColName <- paste0("GF_", prokopack$Site[i], "_rain")
#   dateStartIndex <- which(climateDat$Date==prokopack$date_prokopack[i])
#   prokopack$prior_num_rain_days[i] <- sum(climateDat[(dateStartIndex-6):dateStartIndex, rainColName] > 1)
#   prokopack$prior_num_rain_days_month[i] <- climateDat[which(climateDat$Date==prokopack$date_prokopack[i]), paste0("GF_", prokopack$Site[i], "_rainyDays")]
#   prokopack$prior_cum_rain_1_wk[i] <- climateDat[which(climateDat$Date==prokopack$date_prokopack[i]), paste0("GF_", prokopack$Site[i], "_cumRain")]
#   prokopack$prior_cum_rain_2_wk[i] <- sum(climateDat[(dateStartIndex-13):dateStartIndex, rainColName])
#   prokopack$prior_cum_rain_3_wk[i] <- sum(climateDat[(dateStartIndex-20):dateStartIndex, rainColName])
#   prokopack$prior_cum_rain_4_wk[i] <- sum(climateDat[(dateStartIndex-27):dateStartIndex, rainColName])
# }
# 
# # plot(prokopack$prior_num_rain_days_month, prokopack$total, pch=16)
# plot(prokopack$prior_num_rain_days, prokopack$total, pch=16)
# plot(prokopack$prior_cum_rain_1_wk, prokopack$total, pch=16)
# plot(prokopack$prior_cum_rain_2_wk, prokopack$total, pch=16)
# plot(prokopack$prior_cum_rain_3_wk, prokopack$total, pch=16)
# plot(prokopack$prior_cum_rain_4_wk, prokopack$total, pch=16)
