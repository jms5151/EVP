rm(list=ls()) #remove previous variable assignments

#load libraries
library(plyr)
library(purrr)
library(EpiWeek)
library(openxlsx)
library(reshape2)

# Clinically-diagnosed dengue data from Ecuador 2003-2011 -----------------------------------------
# load data
cases0311 <- read.csv("Ecuador/Case_data/El_Oro_weekly_dengue_2003_2011.csv", head=T, stringsAsFactors = F)
cases0311 <- subset(cases0311, !is.na(YEARS))

# format date
cases0311$Date <- epiweekToDate(cases0311$YEARS, cases0311$AREAS...SEMANAS)[[1]]
cases0311$Date <- as.Date(cases0311$Date, "%Y-%m-$d")
cases0311$Year.Month <- substr(cases0311$Date, 1,7)

# format data from wide to long
weeklyDengue0311 <- melt(cases0311[,c(c("Date", "Year.Month", "Machala", "Huaquillas", "Zaruma.Portovelo.Atahualpa"))], id.vars = c("Date", "Year.Month"))

# format headings
colnames(weeklyDengue0311) <- c("Date", "Year.Month", "Site", "denv_positive")

# Format site name (data came from Portovelo)
weeklyDengue0311$Site <- gsub("Zaruma.Portovelo.Atahualpa", "Portovelo", weeklyDengue0311$Site)

# list all possible site-dates
# dates0311 <- seq.Date(min(weeklyDengue0311$Date), max(weeklyDengue0311$Date), by="week")
# sites <- unique(weeklyDengue0311$Site)
# date.sites.0311 <- expand.grid(Date=dates0311, Site=sites)
  
# save data
write.csv(weeklyDengue0311, "Concatenated_Data/case_data/cases_by_week_Ecuador_2003_2011.csv", row.names = F)

# Laboratory-confirmed dengue data from Ecuador 2017-2018 from MoH reports ----------------------- 
# load data
cases1718 <- read.csv("Ecuador/Case_data/DENV_case_data_MoH_2017_2018.csv", head=T)

# format dates
cases1718$Year <- as.numeric(substr(cases1718$Epiweek, 1,4))
cases1718$wk <- as.numeric(substr(cases1718$Epiweek, 6,8))
cases1718$Date <- epiweekToDate(cases1718$Year, cases1718$wk)[[1]]
cases1718$Date <- as.Date(cases1718$Date, "%Y-%m-$d")
cases1718$Year.Month <- substr(cases1718$Date, 1,7)

# remove cases before 2016 due to poor data quality
cases1718 <- subset(cases1718, Year == 2017|Year==2018)

# subset data
cases1718 <- cases1718[,c("Date", "Year.Month", "Site", "denv_positive")]

# list all possible site-dates
dates1718 <-seq.Date(min(cases1718$Date), max(cases1718$Date), by="week")
sites <- unique(cases1718$Site)
date.sites.1718 <- expand.grid(Date=dates1718, Site=sites)

# merge site-dates with dengue data
cases1718 <- merge(cases1718, date.sites.1718, by=c("Date", "Site"), all=T)

# save data
write.csv(cases1718, "Concatenated_Data/case_data/cases_by_week_Ecuador_2017_2018.csv", row.names = F)

# -----------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------
# Laboratory-confirmed and unconfirmed weekly dengue cases and lab-confirmed weekly chikungunya cases from Machala, Huaquillas, Portovelo, and Zaruma from 2014-2018 from MoH (pers. comm.)
filename <-"C:/Users/Jamie/Box Sync/DENV/Ecuador/Case_data/DENV_El_Oro_cohort_cities_2014-2018.xlsx"
sheets <- openxlsx::getSheetNames(filename)
SheetList <- lapply(sheets,openxlsx::read.xlsx,xlsxFile=filename)
names(SheetList) <- sheets

# list sheet names
noWSList <- SheetList[grep("noWS", sheets)]
withWSList <- SheetList[grep("withWS", sheets)]

# cancatenated unconfirmed and confirmed dengue data into separate dataframes
unconfirmed <- do.call(rbind, lapply(noWSList, subset, select=c("Año", "Semana", "Canton", "Total")))
confirmed <- do.call(rbind, lapply(withWSList, subset, select=c("Fec.atencion", "Canton.Domic")))

# format dates
unconfirmed$Date <- epiweekToDate(unconfirmed$Año, unconfirmed$Semana)[[1]]
unconfirmed$Date <- as.Date(unconfirmed$Date, "%Y-%m-$d")
confirmed$Date <- convertToDate(confirmed$Fec.atencion, origin = "1900-01-01")
date <- dateToEpiweek(confirmed$Date)
date <- epiweekToDate(date$year, date$weekno)[[1]]
confirmed$Date <- as.Date(date, "%Y-%m-%d")

# rename canton in confirmed
colnames(confirmed)[2] <- "Canton"

# summarize by date
weekly.unconfirmed <- ddply(unconfirmed, .(Date, Canton), summarize, denv_positive_clinically_diagnosed = sum(Total))
weekly.confirmed <- ddply(confirmed, .(Date, Canton), summarize, denv_positive_lab_confirmed = length(Canton))

# merge data
weeklyDengue1418 <- merge(weekly.confirmed, weekly.unconfirmed, by=c("Date", "Canton"), all=T)

# format headings
colnames(weeklyDengue1418)[2] <- "Site"

# adjust dengue values for 2015 based on Lowe et al. 2017 https://ars.els-cdn.com/content/image/1-s2.0-S2542519617300645-mmc1.pdf
weeklyDengue1418$Year.Month <- substr(weeklyDengue1418$Date, 1, 7)
weeklyDengue1418$denv_positive_adjusted <- weeklyDengue1418$denv_positive_clinically_diagnosed
weeklyDengue1418$denv_positive_adjusted <- ifelse(weeklyDengue1418$Year.Month == "2015-03", round(weeklyDengue1418$denv_positive_clinically_diagnosed * (1-0.143)), weeklyDengue1418$denv_positive_adjusted) 
weeklyDengue1418$denv_positive_adjusted <- ifelse(weeklyDengue1418$Year.Month == "2015-04", round(weeklyDengue1418$denv_positive_clinically_diagnosed * (1-0.105)), weeklyDengue1418$denv_positive_adjusted) 
weeklyDengue1418$denv_positive_adjusted <- ifelse(weeklyDengue1418$Year.Month == "2015-05", round(weeklyDengue1418$denv_positive_clinically_diagnosed * (1-0.432)), weeklyDengue1418$denv_positive_adjusted) 
weeklyDengue1418$denv_positive_adjusted <- ifelse(weeklyDengue1418$Year.Month == "2015-06", round(weeklyDengue1418$denv_positive_clinically_diagnosed * (1-0.60)), weeklyDengue1418$denv_positive_adjusted) 
weeklyDengue1418$denv_positive_adjusted <- ifelse(weeklyDengue1418$Year.Month == "2015-07", round(weeklyDengue1418$denv_positive_clinically_diagnosed * (1-0.65)), weeklyDengue1418$denv_positive_adjusted) 
weeklyDengue1418$denv_positive_adjusted <- ifelse(weeklyDengue1418$Year.Month == "2015-08", round(weeklyDengue1418$denv_positive_clinically_diagnosed * (1-0.70)), weeklyDengue1418$denv_positive_adjusted) 
weeklyDengue1418$denv_positive_adjusted <- ifelse(weeklyDengue1418$Year.Month == "2015-09", round(weeklyDengue1418$denv_positive_clinically_diagnosed * (1-0.45)), weeklyDengue1418$denv_positive_adjusted) 

# add chikugunya data
chikv1518 <- read.csv("Ecuador/Case_data/CHIKV_El_Oro_cohort_cities_2015-2018.csv", head=T, stringsAsFactors = F)

# format dates
chikv1518$Fec.atencion <- as.Date(chikv1518$Fec.atencion, "%m/%d/%Y")
date <- dateToEpiweek(chikv1518$Fec.atencion)
date <- epiweekToDate(date$year, date$weekno)[[1]]
chikv1518$Date <- as.Date(date, "%Y-%m-%d")

# summarize chikungunya cases 
chikv1518 <- ddply(chikv1518, .(Date, Canton..Domic), summarize, chikv_positive = sum(Casos, na.rm=T))

# adjust chikungunya values based on Lowe et al. 2017 
chikv1518$Year.Month <- substr(chikv1518$Date, 1, 7)
chikv1518$chikv_positive_adjusted <- chikv1518$chikv_positive
chikv1518$chikv_positive_adjusted <- ifelse(chikv1518$Year.Month == "2015-03", round(chikv1518$chikv_positive * 1.143), chikv1518$chikv_positive_adjusted) 
chikv1518$chikv_positive_adjusted <- ifelse(chikv1518$Year.Month == "2015-04", round(chikv1518$chikv_positive * 1.105), chikv1518$chikv_positive_adjusted) 
chikv1518$chikv_positive_adjusted <- ifelse(chikv1518$Year.Month == "2015-05", round(chikv1518$chikv_positive * 1.432), chikv1518$chikv_positive_adjusted) 
chikv1518$chikv_positive_adjusted <- ifelse(chikv1518$Year.Month == "2015-06", round(chikv1518$chikv_positive * 1.60), chikv1518$chikv_positive_adjusted) 
chikv1518$chikv_positive_adjusted <- ifelse(chikv1518$Year.Month == "2015-07", round(chikv1518$chikv_positive * 1.65), chikv1518$chikv_positive_adjusted) 
chikv1518$chikv_positive_adjusted <- ifelse(chikv1518$Year.Month == "2015-08", round(chikv1518$chikv_positive * 1.70), chikv1518$chikv_positive_adjusted) 
chikv1518$chikv_positive_adjusted <- ifelse(chikv1518$Year.Month == "2015-09", round(chikv1518$chikv_positive * 1.45), chikv1518$chikv_positive_adjusted) 

# add Zika data
zikv1618 <- read.csv("Ecuador/Case_data/ZIKV_El_Oro_cohort_cities_2016-2018.csv", head=T, stringsAsFactors = F)

# format dates
zikv1618$Fec.atencion <- as.Date(zikv1618$Fec.atencion, "%m/%d/%Y")
date <- dateToEpiweek(zikv1618$Fec.atencion)
date <- epiweekToDate(date$year, date$weekno)[[1]]
zikv1618$Date <- as.Date(date, "%Y-%m-%d")
zikv1618$Year.Month <- substr(zikv1618$Date, 1, 7)

# remove non-Zika cases
zikv1618 <- subset(zikv1618, Diagnostico.final == "ZIKA")

# summarize zika cases 
zikv1618 <- ddply(zikv1618, .(Date, Year.Month, Canton..Domic), summarize, zikv_positive = sum(CASOS, na.rm=T))

# merge chikungunya and Zika data
chikv.zikv <- merge(chikv1518, zikv1618, by=c("Date", "Canton..Domic", "Year.Month"), all=T)

# rename canton as site
colnames(chikv.zikv)[2] <- "Site"

# merge chikungunya and Zika data with dengue data
denv.chikv.zikv <- merge(weeklyDengue1418, chikv.zikv, by=c("Date", "Site", "Year.Month"), all=T)

# add Year.Month for all data
denv.chikv.zikv$Year.Month <- substr(denv.chikv.zikv$Date, 1, 7)

# format site column
denv.chikv.zikv$Site <- paste0(toupper(substr(denv.chikv.zikv$Site, 1, 1)), tolower(substr(denv.chikv.zikv$Site, 2, nchar(denv.chikv.zikv$Site))))

# add missing weeks
allweeks <- seq.Date(min(denv.chikv.zikv$Date), max(denv.chikv.zikv$Date), by="week")
allsites <- unique(denv.chikv.zikv$Site)
weeksites <- as.data.frame(expand.grid("Date"=allweeks, "Site"=allsites))
denv.chikv.zikv <- merge(denv.chikv.zikv, weeksites, by=c("Date", "Site"), all=T)

# rearrange columns
denv.chikv.zikv <- denv.chikv.zikv[,c("Date", "Year.Month", "Site", "denv_positive_lab_confirmed", "denv_positive_clinically_diagnosed", "denv_positive_adjusted", "chikv_positive", "chikv_positive_adjusted", "zikv_positive")]

# save
write.csv(denv.chikv.zikv, "Concatenated_Data/case_data/cases_by_week_Ecuador_2014_2018.csv", row.names = F)

# # Unconfirmed (?) monthly dengue cases from Machala from 2002-2016 ------------------------------------------------
# machala0216 <- read.csv("Ecuador/Case_data/Machala_monthlydengue_2002-2016.csv", head=T)
# machala0211 <- subset(machala0216, year <= 2011)
# 
# # Unconfirmed (?) weekly dengue cases from Machala, Huaquillas, and Portovelo/Zaruma combined from 2002-2011 ------
# eloro0211 <- read.csv("Ecuador/Case_data/El_Oro_weekly_dengue_2003_2011.csv", head=T)
# eloro0211$Date <- epiweekToDate(eloro0211$YEARS, eloro0211$AREAS...SEMANAS)[[1]]
# eloro0211$Date <- as.Date(eloro0211$Date, "%Y-%m-$d")
# eloro0211$month <- as.integer(substr(eloro0211$Date, 6,7))
# eloro0211$year <- as.integer(eloro0211$YEARS)
# eloro0211monthly <- ddply(eloro0211, .(year, month), summarize, machala_monthly_cases_summed = sum(Machala))
# compare machala monthly to weekly
# machala.comparison <- merge(machala0211[,c("year", "month", "cases")], eloro0211monthly, by=c("year", "month"))
# machala.comparison$difference <- machala.comparison$machala_monthly_cases_summed - machala.comparison$cases
# eloro0211 <- eloro0211[,c("Date", "Machala", "Huaquillas", "Zaruma.Portovelo.Atahualpa")]