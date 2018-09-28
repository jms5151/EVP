rm(list=ls()) #remove previous variable assignments

# packages -----------------------------------------------------------------
library(dplyr)
library(plyr)
library(redcapAPI)
library(REDCapR)

# coordinates for people -----------------------------------------------------------------
source("C:/Users/Jamie/Box Sync/R_functions/REDCap_API_Token_Case_Data.R")
REDcap.URL  <- 'https://redcap.stanford.edu/api/'
rcon <- redcapConnection(url=REDcap.URL, token=Redcap.token)

#export data from redcap to R (must be connected via cisco VPN)
redcap_data <- redcap_read(redcap_uri  = REDcap.URL, token = Redcap.token, batch_size = 300)$data

R01_lab_results <- redcap_data #leave original dataset as backup

# AIC coordinates -----------------
aicGPS <- R01_lab_results[,c("person_id", "aic_village_gps_lattitude", "aic_village_gps_longitude")]
aicGPS <- aicGPS[!duplicated(aicGPS), ]
aicGPS <- aicGPS[!is.na(aicGPS$aic_village_gps_lattitude),]

aicGPS$filled.rows <- rowSums(!is.na(aicGPS))
aicGPS2 <- aicGPS %>% group_by(person_id) %>% slice(which.max(filled.rows))
aicGPS2 <- aicGPS2[,-grep("filled.rows",colnames(aicGPS2))]
aicGPS2 <- as.data.frame(aicGPS2)

names(aicGPS2) <- c("ID", "longitude", "latitude")
aicGPS2$ID_Type <- "person_id_aic"

# connect aic coordinates with case data
aic.casedata <- read.csv("Concatenated_Data/aic_timeline.csv", head = T, stringsAsFactors = F)
aic.casedata <- aic.casedata[,c("person_id", "id_site", "visit_a_infected_denv_stfd", "visit_a_infected_chikv_stfd")]
colnames(aic.casedata) <- c("person_id", "id_site", "denv_positive", "chikv_positive")
aic.cases.with.gps <- merge(aicGPS2, aic.casedata, by.x = "ID", by.y = "person_id")

# HCC coordinates -----------------
hccGPS <- R01_lab_results[,c("person_id", "dem_house_longitude", "dem_house_latitude")]
hccGPS <- hccGPS[!duplicated(hccGPS), ]
hccGPS <- hccGPS[!is.na(hccGPS$dem_house_latitude),]
names(hccGPS) <- c("ID", "longitude", "latitude")
hccGPS$ID_Type <- "person_id_hcc"

hcc.casedata <- read.csv("Concatenated_Data/hcc_results.csv", head=T, stringsAsFactors = F)
hcc.casedata$denv_positive <- ifelse(rowSums(hcc.casedata[,c(27:34)]) > 0, 1, 0)
hcc.casedata$chikv_positive <- ifelse(rowSums(hcc.casedata[,c(35:42)]) > 0, 1, 0)
hcc.casedata <- hcc.casedata[,c("person_id", "id_site", "denv_positive", "chikv_positive")]
hcc.casedata <- hcc.casedata[!is.na(hcc.casedata$denv_positive),]

hcc.cases.with.gps <- merge(hccGPS, hcc.casedata, by.x = "ID", by.y = "person_id")

# merge cohort data --------------
cohortGPS <- rbind(aic.cases.with.gps, hcc.cases.with.gps)
decdegrees <- subset(cohortGPS, longitude < 100)

# convert utm to decimal degrees
library(rgdal) 
utmWest <- subset(cohortGPS, longitude > 600000 & latitude > 9600000)
# utmWest <- subset(cohortGPS, id_site == "Chulaimbo" | id_site == "Kisumu")
# utmWest <- utmWest[!is.na(utmWest$latitude),]
colnames(utmWest)[2] <-  "Easting"
colnames(utmWest)[3] <-  "Northing"

utmcoor<-SpatialPoints(cbind(utmWest$Easting,utmWest$Northing), proj4string=CRS("+proj=utm +zone=36+south=T"))
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))
utmWest$longitude <- coordinates(longlatcoor)[,1]
utmWest$latitude <- coordinates(longlatcoor)[,2]

# utmWest$utm_zone <- "36S"
utmCoast <- subset(cohortGPS, longitude > 500000 & longitude < 600000)
# utmCoast$utm_zone <- "37S"
# utm <- rbind(utmWest, utmCoast)
colnames(utmCoast)[2] <-  "Easting"
colnames(utmCoast)[3] <-  "Northing"

utmcoor<-SpatialPoints(cbind(utmCoast$Easting,utmCoast$Northing), proj4string=CRS("+proj=utm +zone=37+south=T"))
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))
utmCoast$longitude <- coordinates(longlatcoor)[,1]
utmCoast$latitude <- coordinates(longlatcoor)[,2]

  
# coordinates for vector houses -----------------------------------------------------------------
source("C:/Users/Jamie/Box Sync/R_functions/REDCap_API_Token_Climate_and_Vector_Data.R")
rcon <- redcapConnection(url=REDcap.URL, token=clim.vec.token)
redcap_clim_vec <- redcap_read(redcap_uri  = REDcap.URL, token = clim.vec.token, batch_size = 300)$data

redcap_clim_vec$vector_house_id <- substr(redcap_clim_vec$redcap_event_name, 1,4)
redcap_clim_vec$vector_house_id <- as.numeric(redcap_clim_vec$vector_house_id)
gpsHouses <- redcap_clim_vec[,c("vector_house_id", "longitude", "latitude")]
gpsHouses2 <- gpsHouses[!duplicated(gpsHouses),]
gpsHouses3 <- gpsHouses2[!is.na(gpsHouses2$longitude),]
gpsHouses3$utm_zone = NA
names(gpsHouses3) <- c("ID", "longitude", "latitude", "utm_zone")
gpsHouses3$ID_Type <- "house_id"

# combine people and vector house coordinates ----------------------------------------------------
coordinates <- rbind(aicGPS2, gpsHouses3)
coordinates$utm_zone <- gsub(1, "36S", coordinates$utm_zone)
coordinates$utm_zone <- gsub(2, "37S", coordinates$utm_zone)

write.csv(coordinates, "kenya_gps_coordinates.csv", row.names = F)