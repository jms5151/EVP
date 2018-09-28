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

# denvPos <- R01_lab_results[,c("person_id", "redcap_event_name", "result_pcr_denv_kenya", "serotype_pcr_denv_kenya___1"
#                       , "serotype_pcr_denv_kenya___2", "serotype_pcr_denv_kenya___3"
#                       , "serotype_pcr_denv_kenya___4", "int_date")]
# denvPos <- subset(denvPos, result_pcr_denv_kenya == 1)
# plot(denvPos$int_date, denvPos$serotype_pcr_denv_kenya___4)
# denv <- subset(R01_lab_results, result_pcr_denv_kenya == 1)
# denv1 <- denv[,c("person_id", "result_pcr_denv_kenya", "int_date", "serotype_pcr_denv_kenya___1")]
# denv2 <- denv[,c("person_id", "result_pcr_denv_kenya", "int_date", "serotype_pcr_denv_kenya___2")]
# denv3 <- denv[,c("person_id", "result_pcr_denv_kenya", "int_date", "serotype_pcr_denv_kenya___3")]
# denv4 <- denv[,c("person_id", "result_pcr_denv_kenya", "int_date", "serotype_pcr_denv_kenya___4")]
# 
# colnames(denv1)[4]<-"serotype"
# denv1$serotype <- ifelse(denv1$serotype == 1, 1, 0)
# colnames(denv2)[4]<-"serotype"
# denv2$serotype <- ifelse(denv1$serotype == 1, 2, 0)
# colnames(denv3)[4]<-"serotype"
# denv3$serotype <- ifelse(denv1$serotype == 1, 3, 0)
# colnames(denv4)[4]<-"serotype"
# denv4$serotype <- ifelse(denv1$serotype == 1, 4, 0)
# 
# denvPos <- rbind(denv1, denv2, denv3, denv4)
# denvPos <- subset(denvPos, serotype != 0)
# denvPos$serotype <- as.factor(denvPos$serotype)
# 
# ggplot() + geom_bar(aes(y = serotype, x = int_date, fill = serotype), data = denvPos,
#                     stat="identity")
