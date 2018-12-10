# Concatenate laboratory-confirmed case data from Ecuador --------------------------- 
library(EpiWeek)

# load data
ec.cases <- read.csv("Ecuador/case_data.csv", head=T)

# format dates
ec.cases$Year <- as.numeric(substr(ec.cases$Epiweek, 1,4))
ec.cases$wk <- as.numeric(substr(ec.cases$Epiweek, 6,8))
ec.cases$Date <- epiweekToDate(ec.cases$Year, ec.cases$wk)[[1]]
ec.cases$Date <- as.Date(ec.cases$Date, "%Y-%m-$d")
ec.cases$Year.Month <- substr(ec.cases$Date, 1,7)

# remove cases before 2016 due to poor data quality
ec.cases <- subset(ec.cases, Year >= 2017)

# save data
write.csv(ec.cases, "Concatenated_Data/case_data/cases_by_week_Ecuador.csv", row.names = F)

# summarize by Month.Year
cases <- ddply(ec.cases, .(Site, Year.Month), summarize, denv_positive=sum(denv_positive, na.rm=T), Date = max(Date))

# save data
write.csv(cases, "Concatenated_Data/case_data/cases_by_month_Ecuador.csv", row.names = F)

# Clinical case data from Ecuador 2003-2011 -----------------------------------------
# load data
cases0311 <- read.csv("Ecuador/Case_data/El_Oro_weekly_dengue_2003_2011.csv", head=T, stringsAsFactors = F)
cases0311 <- subset(cases0311, !is.na(YEARS))

# format date
cases0311$Date <- epiweekToDate(cases0311$YEARS, cases0311$AREAS...SEMANAS)[[1]]
cases0311$Date <- as.Date(cases0311$Date, "%Y-%m-$d")
cases0311$Year.Month <- substr(cases0311$Date, 1,7)

# save data
write.csv(cases0311, "Concatenated_Data/case_data/cases_by_week_Ecuador_2003-2011.csv", row.names = F)
