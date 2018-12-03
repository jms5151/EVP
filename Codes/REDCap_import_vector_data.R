rm(list=ls()) #remove previous variable assignments

# packages -----------------------------------------------------------------
library(redcapAPI)
library(REDCapR)

# get data -----------------------------------------------------------------
source("C:/Users/Jamie/Box Sync/R_functions/REDCap_API_Token_Vector_Data.R")
REDcap.URL  <- 'https://redcap.stanford.edu/api/'
rcon <- redcapConnection(url=REDcap.URL, token=vector.token)

#export data from redcap to R (must be connected via cisco VPN)
redcap_vector <- redcap_read(redcap_uri  = REDcap.URL, token = vector.token, batch_size = 300)$data
vector_backup <- redcap_vector

