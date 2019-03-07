# Concatenate vector data from Ecuador field sites ---------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(plyr)

# load functions
source("C:/Users/Jamie/Box Sync/R_functions/date_from_week_number.R")

# load data
load("Ecuador/EVP_Ecuador_Data/vector_data/h_mosq_new.RData")
load("Ecuador/EVP_Ecuador_Data/vector_data/m_mosq_new.RData")
load("Ecuador/EVP_Ecuador_Data/vector_data/p_mosq_new.RData")
load("Ecuador/EVP_Ecuador_Data/vector_data/z_mosq_new.RData")

# combine all data into list
mosquitoes.ecuador <- list(hall6, mall6, pall6, zall6)

# add site information; 1=h, 2=m, 3=p, 4=z
ecuador.sites <- list(1,2,3,4) #"h", "m", "p", "z"
mosquitoes.ecuador <- Map(cbind, mosquitoes.ecuador, Site = ecuador.sites)

# row bind list of dataframes in single dataframe
mosquitoes.ecuador <- do.call("rbind", mosquitoes.ecuador)

# create date from year and and week of year
mosquitoes.ecuador$Date <- calculate_end_of_week(mosquitoes.ecuador$week, mosquitoes.ecuador$year)

# get average number of mosquitoes per house
mosquitoes.ecuador$aedes_total <- round(mosquitoes.ecuador$totA/mosquitoes.ecuador$houses)

# rename sites
mosquitoes.ecuador$Site[mosquitoes.ecuador$Site==1] <- "Huaquillas"
mosquitoes.ecuador$Site[mosquitoes.ecuador$Site==2] <- "Machala"
mosquitoes.ecuador$Site[mosquitoes.ecuador$Site==3] <- "Portovelo"
mosquitoes.ecuador$Site[mosquitoes.ecuador$Site==4] <- "Zaruma"

# save data
write.csv(mosquitoes.ecuador[,c("Site", "Date", "aedes_total")], "Concatenated_Data/vector_data/vector_data_Ecuador_weekly.csv", row.names = F)

# summarize by month and year
mosquitoes.ecuador$Year.Month <- substr(mosquitoes.ecuador$Date, 1, 7)
mosquitoes.ecuador2 <- ddply(mosquitoes.ecuador, .(Site, Year.Month), summarize, Date = max(Date), aedes=round(sum(totA)/sum(houses))) 
                             
# save data
write.csv(mosquitoes.ecuador2, "Concatenated_Data/vector_data/vector_data_Ecuador.csv", row.names = F)
