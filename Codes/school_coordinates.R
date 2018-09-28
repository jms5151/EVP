rm(list=ls()) #remove previous variable assignments

# packages -----------------------------------------------------------------
library(dplyr)
library(plyr)
library(redcapAPI)
library(REDCapR)
library(sp)
library(rgdal)
library(ggmap)
library(GISTools)

# coordinates for people -----------------------------------------------------------------
source("C:/Users/Jamie/Box Sync/R_functions/REDCap_API_Token_Case_Data.R")
REDcap.URL  <- 'https://redcap.stanford.edu/api/'
rcon <- redcapConnection(url=REDcap.URL, token=Redcap.token)

#export data from redcap to R (must be connected via cisco VPN)
redcap_data <- redcap_read(redcap_uri  = REDcap.URL, token = Redcap.token, batch_size = 300)$data

R01_lab_results <- redcap_data #leave original dataset as backup

# pull coordinates from google where possible
schools <- sort(unique(R01_lab_results$dem_school_name))
schools.df <- data.frame(school=schools, longitude=as.numeric(NA), latitude=as.numeric(NA))
schools.df <- schools.df[c(2:nrow(schools.df)),] #remove first row which is blank
schools.df$school.ky <- paste0(schools.df$school, ", Kenya")

for (i in 1:nrow(schools.df)){
  coords <- geocode(schools.df$school.ky[i])
  schools.df$longitude[i] <- coords[1]
  schools.df$latitude[i] <- coords[2]
}

# unlist lat/lon columns
library(tidyr)
sch.df <- unnest(schools.df, latitude)
sch.df <- unnest(sch.df, longitude)

# write to csv and fill in rest with google Earth
write.csv(sch.df, "school_coordinates.csv", row.names = F)

# fix capitalization and space erros
sch.df$school.ky <- tolower(sch.df$school.ky)
library(qdapRegex)
sch.df$school.ky <- rm_white(sch.df$school.ky)
length(unique(sch.df$school.ky))
