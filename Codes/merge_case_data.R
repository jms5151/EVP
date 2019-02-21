# merge case data ---------------------------------------
rm(list=ls()) #remove previous variable assignments

library(plyr)

# load case data
cases_kenya <- read.csv("Concatenated_Data/case_data/aic_cases_by_month_Kenya.csv", head=T, stringsAsFactors = F)
cases0311 <- read.csv("Concatenated_Data/case_data/cases_by_week_Ecuador_2003_2011.csv", head=T, stringsAsFactors = F)
cases1418 <- read.csv("Concatenated_Data/case_data/cases_by_week_Ecuador_2014_2018.csv", head=T, stringsAsFactors = F)
cases1718 <- read.csv("Concatenated_Data/case_data/cases_by_week_Ecuador_2017_2018.csv", head=T, stringsAsFactors = F)

# format columns to combine data
cases_kenya$denv_positive_0311 <- NA
cases_kenya$denv_positive_1418 <- NA

cases0311$denv_positive_0311 <- cases0311$denv_positive

cases1418$denv_positive_0311 <- NA
cases1418$denv_positive_1418 <- cases1418$denv_positive_adjusted

# combine case data
cases_ecuador <- merge(cases0311[,c("Site", "Date", "denv_positive_0311")], cases1418[,c("Site", "Date", "chikv_positive", "denv_positive_1418")], by=c("Site", "Date"), all=T)
cases_ecuador <- merge(cases_ecuador, cases1718[,c("Site", "Date", "denv_positive")], by=c("Site", "Date"), all=T)
cases <- rbind(cases_ecuador, cases_kenya[,c("Site", "Date", "denv_positive_0311", "chikv_positive", "denv_positive_1418", "denv_positive")])

# add "weekly" headings
colnames(cases)[3:6] <- paste0(colnames(cases)[3:6], "_weekly")

# calculate monthly estimates
cases$Year.Month <- substr(cases$Date, 1,7)
monthlyCases <- ddply(cases, .(Site, Year.Month)
                      , summarize
                      , Date = min(Date)
                      , chikv_positive_monthly = ifelse(sum(is.na(chikv_positive_weekly))==length(chikv_positive_weekly), NA, sum(chikv_positive_weekly, na.rm=T))
                      , denv_positive_monthly = ifelse(sum(is.na(denv_positive_weekly))==length(denv_positive_weekly), NA, sum(denv_positive_weekly, na.rm=T))
                      , denv_positive_0311_monthly = ifelse(sum(is.na(denv_positive_0311_weekly))==length(denv_positive_0311_weekly), NA, sum(denv_positive_0311_weekly, na.rm=T))
                      , denv_positive_1418_monthly = ifelse(sum(is.na(denv_positive_1418_weekly))==length(denv_positive_1418_weekly), NA, sum(denv_positive_1418_weekly, na.rm=T))
                      )

# merge weekly and monthly data
cases <- merge(cases, monthlyCases, by=c("Site", "Date", "Year.Month"), all=T)
cases$Year.Month <- NULL

# create column for dengue by lab-confirmation or clinical diagnosis
cases$denv_positive_weekly_any <- cases$denv_positive_weekly
cases$denv_positive_weekly_any <- ifelse(is.na(cases$denv_positive_weekly_any) & !is.na(cases$denv_positive_0311_weekly), cases$denv_positive_0311_weekly, cases$denv_positive_weekly_any)
cases$denv_positive_weekly_any <- ifelse(is.na(cases$denv_positive_weekly_any) & !is.na(cases$denv_positive_1418_weekly), cases$denv_positive_1418_weekly, cases$denv_positive_weekly_any)

cases$denv_positive_monthly_any <- cases$denv_positive_monthly
cases$denv_positive_monthly_any <- ifelse(is.na(cases$denv_positive_monthly_any) & !is.na(cases$denv_positive_0311_monthly), cases$denv_positive_0311_monthly, cases$denv_positive_monthly_any)
cases$denv_positive_monthly_any <- ifelse(is.na(cases$denv_positive_monthly_any) & !is.na(cases$denv_positive_1418_monthly), cases$denv_positive_1418_monthly, cases$denv_positive_monthly_any)

# save data
write.csv(cases, "Concatenated_Data/case_data/merged_case_data.csv", row.names = F)
