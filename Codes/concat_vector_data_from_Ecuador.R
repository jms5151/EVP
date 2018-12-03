# Concatenate vector data from Ecuador field sites ---------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(EpiWeek)
library(plyr)

# load data
load("Ecuador/EVP_Ecuador_Data/h_mosq_new.RData")
load("Ecuador/EVP_Ecuador_Data/m_mosq_new.RData")
load("Ecuador/EVP_Ecuador_Data/p_mosq_new.RData")
load("Ecuador/EVP_Ecuador_Data/z_mosq_new.RData")

# combine all data into list
mosquitoes.ecuador <- list(hall6, mall6, pall6, zall6)

# create date from year and and week of year
mosquitoes.ecuador <-lapply(mosquitoes.ecuador, function(x) cbind(x, Date = epiweekToDate(as.numeric(x$year), as.numeric(x$week))[[1]]))

# add site information; 1=h, 2=m, 3=p, 4=z
ecuador.sites <- list(1,2,3,4) #"h", "m", "p", "z"
mosquitoes.ecuador <- Map(cbind, mosquitoes.ecuador, Site = ecuador.sites)

# row bind list of dataframes in single dataframe
mosquitoes.ecuador <- do.call("rbind", mosquitoes.ecuador)

# get average number of mosquitoes per house
mosquitoes.ecuador$aedes_total <- mosquitoes.ecuador$totA/mosquitoes.ecuador$houses

# summarize by month and year
# mosquitoes.ecuador$Year.Month <- substr(mosquitoes.ecuador$Date, 1, 7)
# mosquitoes.ecuador2 <- ddply(mosquitoes.ecuador, .(Site, Year.Month), summarize, Date = max(Date), aedes_total = sum(mean_aedes, na.rm=T))

# rename sites
mosquitoes.ecuador$Site[mosquitoes.ecuador$Site==1] <- "Huaquillas"
mosquitoes.ecuador$Site[mosquitoes.ecuador$Site==2] <- "Machala"
mosquitoes.ecuador$Site[mosquitoes.ecuador$Site==3] <- "Portovelo"
mosquitoes.ecuador$Site[mosquitoes.ecuador$Site==4] <- "Zaruma"

# save data
write.csv(mosquitoes.ecuador, "Concatenated_Data/vector_data/vector_data_Ecuador.csv", row.names = F)
