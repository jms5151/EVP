rm(list=ls()) #remove previous variable assignments

# packages -----------------------------------------------------------------
library(redcapAPI)
library(REDCapR)

# get data -----------------------------------------------------------------
source("C:/Users/Jamie/Box Sync/R_functions/REDCap_API_Token_Climate_and_Vector_Data.R")
REDcap.URL  <- 'https://redcap.stanford.edu/api/'
rcon <- redcapConnection(url=REDcap.URL, token=clim.vec.token)

#export data from redcap to R (must be connected via cisco VPN)
redcap_clim_vec <- redcap_read(redcap_uri  = REDcap.URL, token = clim.vec.token, batch_size = 300)$data
clim_vec_backup <- redcap_clim_vec
redcap_clim_vec[,c("date_collected", "date_set_day_ovitrap")] <- lapply(redcap_clim_vec[,c("date_collected", "date_set_day_ovitrap")], as.Date, "%Y-%m-%d")

