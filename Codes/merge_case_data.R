# merge case data ---------------------------------------
rm(list=ls()) #remove previous variable assignments

# load case data
cases_kenya <- read.csv("Concatenated_Data/case_data/aic_cases_by_month_Kenya.csv", head=T, stringsAsFactors = F)
cases_ecuador <- read.csv("Concatenated_Data/case_data/cases_by_month_Ecuador.csv", head=T, stringsAsFactors = F)

# format columns to combine data
cases_kenya$denv_positive_clinically_diagnosed <- NA
cases_kenya$chikv_positive_clinically_diagnosed <- NA
cases_kenya$chikv_positive <- NA
cases_kenya$zikv_positive <- NA

# merge data
cases <- rbind(cases_ecuador, cases_kenya)

# format date
cases$Date <- as.Date(cases$Date, "%Y-%m-%d")

# remove year-month variable
cases$Year.Month <- NULL

# load Zika data and format
load("Concatenated_Data/CDC_Zika_El_Oro_Total.RData")
cdcZika2$Site <- "Machala"

# merge Zika data with dengue and chikungunya
cases <- merge(cases, cdcZika2[,c("Site", "Date", "confirmed_cases_Zika")], by=c("Site", "Date"), all=T)

# save data
save(cases, file="Concatenated_Data/case_data/merged_case_data.RData", row.names = F)
