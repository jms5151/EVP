# Organize 2010-2011 Ecuador data ------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(plyr)

# load data
pupal.dat <- read.csv("Ecuador/data_2010/Machala_pupal_surveys_2010-2011.csv", head=T)
ovi.dat <- read.csv("Ecuador/data_2010/Machala-ovitrap_data_2010-2011.csv", head=T)

# change spanish names to english
colnames(pupal.dat)[4] <- "Site"
colnames(pupal.dat)[6] <- "Date"
colnames(pupal.dat)[26] <- "Aedes_pupae"
colnames(pupal.dat)[27] <- "Aedes_adults_F"
colnames(pupal.dat)[28] <- "Aedes_adults_M"

pupal.dat[,c("Aedes_adults_F", "Aedes_adults_M")] <- lapply(pupal.dat[,c("Aedes_adults_F", "Aedes_adults_M")], as.numeric)

# set dates
pupal.dat$Date <- as.Date(pupal.dat$Date, "%m/%d/%Y")
pupal.dat$mn.yr <- format(pupal.dat$Date, "%Y-%m")

# sum mosquito abundances by month-year
# wide range of number of houses per barrio, need to ask differences between barrio and barrio2
ec.abund <- ddply(pupal.dat, .(mn.yr) # Site == PE, HJ,VD
                  , summarise
                  , pupae_total = sum(Aedes_pupae, na.rm=T)
                  , adults_total = sum(Aedes_adults_F, Aedes_adults_M, na.rm = T)
                  , num_houses = length(unique(Casa.codigo))
                  , mean_pupae = pupae_total/num_houses
                  , mean_adults = adults_total/num_houses)

surveysByHouse <- ddply(pupal.dat, .(Site) # Site == PE, HJ,VD
                  , summarise
                  , num_visits = length(unique(Date)))
