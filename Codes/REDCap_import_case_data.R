# packages -----------------------------------------------------------------
library(redcapAPI)
library(REDCapR)

# get data -----------------------------------------------------------------
source("C:/Users/Jamie/Box Sync/R_functions/REDCap_API_Token_Case_Data.R")
REDcap.URL  <- 'https://redcap.stanford.edu/api/'
rcon <- redcapConnection(url=REDcap.URL, token=Redcap.token)

#export data from redcap to R (must be connected via cisco VPN)
redcap_data <- redcap_read(redcap_uri  = REDcap.URL, token = Redcap.token, batch_size = 300)$data

R01_lab_results <- redcap_data #leave original dataset as backup
