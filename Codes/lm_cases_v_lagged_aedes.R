# plot model simulations with vector and case data ----------------------------
rm(list=ls()) #remove previous variable assignments

# load case data
cases <- read.csv("Concatenated_Data/case_data/merged_case_data.csv", head=T, stringsAsFactors = F)

# load vector data
vectors <- read.csv("Concatenated_Data/vector_data/merged_vector_data.csv", head=T, stringsAsFactors = F)

# combine case data with vector data
library(plyr)

data <- merge(cases, vectors, by=c("Site", "Date", "Year.Month"), all=T)

data2 <- ddply(data, .(Site, Year.Month), summarize
               , denv_positive_CD = ifelse(all(is.na(denv_positive_clinically_diagnosed)), NA, sum(denv_positive_clinically_diagnosed, na.rm=T))
               , denv_positive_LC = ifelse(all(is.na(denv_positive)), NA, sum(denv_positive, na.rm=T))
               , chikv_positive = ifelse(all(is.na(chikv_positive)), NA, sum(chikv_positive, na.rm=T))
               , aedes_total = ifelse(all(is.na(aedes_total)), NA, sum(aedes_total, na.rm=T)))

# lag mosquito values
library(dplyr)

data2 <- data2 %>%
  group_by(Site) %>%
  mutate(aedes_total_lag_1month = dplyr::lag(aedes_total, n = 1, default = NA))

# separate data by country
kenya <- subset(data2, Site=="Chulaimbo"|Site=="Kisumu"|Site=="Msambweni"|Site=="Ukunda")
ecuador <- subset(data2, Site=="Huaquillas"|Site=="Machala"|Site=="Portovelo"|Site=="Zaruma")

# linear regressions to determine whether dengue or chik are functions of mosquitoes at 1 month lagged
kenya_denv <- lm(denv_positive_LC~aedes_total_lag_1month+Site, data=kenya)
summary(kenya_denv)

kenya_chikv <- lm(chikv_positive~aedes_total_lag_1month+Site, data=kenya)
summary(kenya_chikv)

ecuador_denv_LC <- lm(denv_positive_LC~aedes_total_lag_1month+Site, data=ecuador)
summary(ecuador_denv_LC)

ecuador_denv_CD <- lm(denv_positive_CD~aedes_total_lag_1month+Site, data=ecuador)
summary(ecuador_denv_CD)

ecuador_chikv <- lm(chikv_positive~aedes_total_lag_1month+Site, data=ecuador)
summary(ecuador_chikv)
