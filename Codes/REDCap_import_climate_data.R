rm(list=ls()) #remove previous variable assignments

# packages -----------------------------------------------------------------
library(redcapAPI)
library(REDCapR)

# get data -----------------------------------------------------------------
source("C:/Users/Jamie/Box Sync/R_functions/REDCap_API_Token_Climate_Data.R")
REDcap.URL  <- 'https://redcap.stanford.edu/api/'
rcon <- redcapConnection(url=REDcap.URL, token=climate.token)

#export data from redcap to R (must be connected via cisco VPN)
redcap_climate <- redcap_read(redcap_uri  = REDcap.URL, token = climate.token, batch_size = 300)$data
climate_backup <- redcap_climate
redcap_climate$date_collected <- as.Date(redcap_climate$date_collected, "%Y-%m-%d")

