# concatenate case data for dengue, chikungunya, and zika for Ecuador -----------------------------
rm(list=ls()) #remove previous variable assignments

#### Dengue cases:
#### 2002-01-01 - 2016-12-31 for Machala (clinically-diagnosed)
#### 2003-01-07 - 2011-12-30 for Machala, Huaquillas, Portovelo (clinically diagnosed)
#### 2014-01-14 - 2018-08-19 for Machala, Huaquillas, Portovelo, Zaruma (laboratory-confirmed [data really only for Machala] and clinically diagnosed)
#### 2017-01-07 - 2018-10-21 for Machala, Huaquillas, Portovelo, Zaruma (laboratory-confirmed)

#### Chikungunya cases:
#### 2015-04-06 - 2018-08-04 for Machala, Huaquillas, Zaruma (laboratory-confirmed)

#### Zika cases:
#### 2016-01-22 - 2018-01-03 for Machala (laboratory-confirmed)

#load libraries
library(plyr)
library(purrr)
library(openxlsx)
library(reshape2)

# load functions
source("C:/Users/Jamie/Box Sync/R_functions/date_from_week_number.R")

# Clinically-diagnosed dengue data from Ecuador 2003-2011 -----------------------------------------
# load data
cases0311 <- read.csv("Ecuador/Case_data/El_Oro_weekly_dengue_2003_2011.csv", head=T, stringsAsFactors = F)
cases0311 <- subset(cases0311, !is.na(YEARS))

# format date
cases0311$Date <- calculate_end_of_week(cases0311$AREAS...SEMANAS, cases0311$YEARS)

# format data from wide to long
cases0311 <- melt(cases0311[,c(c("Date", "Machala", "Huaquillas", "Zaruma.Portovelo.Atahualpa"))], id.vars = c("Date"))

# format headings
colnames(cases0311) <- c("Date", "Site", "denv_positive_clinically_diagnosed")

# Format site name (data came from Portovelo)
cases0311$Site <- gsub("Zaruma.Portovelo.Atahualpa", "Portovelo", cases0311$Site)

# add dataset name 
# cases0311$dataset <- "El Oro 2003-2011"

# save data
# write.csv(weeklyDengue0311, "Concatenated_Data/case_data/cases_by_week_Ecuador_2003_2011.csv", row.names = F)

# Laboratory-confirmed dengue data from Ecuador 2017-2018 from MoH reports ----------------------- 
# load data
cases1718 <- read.csv("Ecuador/Case_data/DENV_case_data_MoH_2017_2018.csv", head=T)

# format dates
cases1718$Year <- as.numeric(substr(cases1718$Epiweek, 1,4))
cases1718$wk <- as.numeric(substr(cases1718$Epiweek, 6,8))
cases1718$Date <- calculate_end_of_week(cases1718$wk, cases1718$Year)

# remove cases before 2016 due to poor data quality
cases1718 <- subset(cases1718, Year == 2017|Year==2018)

# subset data
cases1718 <- cases1718[,c("Date", "Site", "denv_positive")]

# add dataset name
# cases1718$dataset <- "MoH 2016-2018"

# save data
# write.csv(cases1718, "Concatenated_Data/case_data/cases_by_week_Ecuador_2017_2018.csv", row.names = F)

# Laboratory-confirmed and unconfirmed weekly dengue cases and lab-confirmed weekly chikungunya cases from Machala, Huaquillas, Portovelo, and Zaruma from 2014-2018 from MoH (pers. comm.)
filename <-"Ecuador/Case_data/DENV_El_Oro_cohort_cities_2014-2018.xlsx"
sheets <- openxlsx::getSheetNames(filename)
SheetList <- lapply(sheets,openxlsx::read.xlsx,xlsxFile=filename)
names(SheetList) <- sheets

# list sheet names
noWSList <- SheetList[grep("noWS", sheets)]
withWSList <- SheetList[grep("withWS", sheets)]

# cancatenated unconfirmed and confirmed dengue data into separate dataframes
unconfirmed1418 <- do.call(rbind, lapply(noWSList, subset, select=c("Año", "Semana", "Canton", "Total")))
confirmed1418 <- do.call(rbind, lapply(withWSList, subset, select=c("Fec.atencion", "Canton.Domic")))

# format dates
unconfirmed1418$Date <- calculate_end_of_week(unconfirmed1418$Semana, unconfirmed1418$Año)
confirmed1418$Date <- convertToDate(confirmed1418$Fec.atencion, origin = "1900-01-01")

# summarize by date
unconfirmed1418 <- ddply(unconfirmed1418, .(Date, Canton), summarize, denv_positive_clinically_diagnosed = sum(Total))
confirmed1418 <- ddply(confirmed1418, .(Date, Canton.Domic), summarize, denv_positive = length(Canton.Domic))

# rename canton to site
unconfirmed1418$Site <-  paste0(substr(unconfirmed1418$Canton, 1, 1), tolower(substr(unconfirmed1418$Canton, 2, nchar(unconfirmed1418$Canton))))
confirmed1418$Site <-  paste0(substr(confirmed1418$Canton, 1, 1), tolower(substr(confirmed1418$Canton, 2, nchar(confirmed1418$Canton))))

# remove cantons
unconfirmed1418$Canton <- NULL
confirmed1418$Canton.Domic <- NULL

# adjust dengue values for 2015 based on Lowe et al. 2017 
# https://ars.els-cdn.com/content/image/1-s2.0-S2542519617300645-mmc1.pdf
unconfirmed1418$Year.Month <- substr(unconfirmed1418$Date, 1, 7)
unconfirmed1418$denv_positive_clinically_diagnosed <- ifelse(unconfirmed1418$Year.Month == "2015-03", round(unconfirmed1418$denv_positive_clinically_diagnosed * (1-0.143)), unconfirmed1418$denv_positive_clinically_diagnosed) 
unconfirmed1418$denv_positive_clinically_diagnosed <- ifelse(unconfirmed1418$Year.Month == "2015-04", round(unconfirmed1418$denv_positive_clinically_diagnosed * (1-0.105)), unconfirmed1418$denv_positive_clinically_diagnosed) 
unconfirmed1418$denv_positive_clinically_diagnosed <- ifelse(unconfirmed1418$Year.Month == "2015-05", round(unconfirmed1418$denv_positive_clinically_diagnosed * (1-0.432)), unconfirmed1418$denv_positive_clinically_diagnosed) 
unconfirmed1418$denv_positive_clinically_diagnosed <- ifelse(unconfirmed1418$Year.Month == "2015-06", round(unconfirmed1418$denv_positive_clinically_diagnosed * (1-0.60)), unconfirmed1418$denv_positive_clinically_diagnosed) 
unconfirmed1418$denv_positive_clinically_diagnosed <- ifelse(unconfirmed1418$Year.Month == "2015-07", round(unconfirmed1418$denv_positive_clinically_diagnosed * (1-0.65)), unconfirmed1418$denv_positive_clinically_diagnosed) 
unconfirmed1418$denv_positive_clinically_diagnosed <- ifelse(unconfirmed1418$Year.Month == "2015-08", round(unconfirmed1418$denv_positive_clinically_diagnosed * (1-0.70)), unconfirmed1418$denv_positive_clinically_diagnosed) 
unconfirmed1418$denv_positive_clinically_diagnosed <- ifelse(unconfirmed1418$Year.Month == "2015-09", round(unconfirmed1418$denv_positive_clinically_diagnosed * (1-0.45)), unconfirmed1418$denv_positive_clinically_diagnosed) 
unconfirmed1418$Year.Month <- NULL

# add dataset names
# unconfirmed1418$dataset <- "MoH 2014-2018"
# confirmed1418$dataset <- "MoH 2014-2018"

# remove overlapping data
confirmed1416 <- subset(confirmed1418, Date < min(cases1718$Date))

# Unconfirmed monthly dengue cases from Machala from 2002-2016 ------------------------------------------------
# load data
machala0216 <- read.csv("Ecuador/Case_data/Machala_monthlydengue_2002-2016.csv", head=T)

# format dates
machala0216$month <- as.character(machala0216$month)
machala0216$month <- ifelse(nchar(machala0216$month)==1, paste0("0", machala0216$month), machala0216$month)
machala0216$Date <- paste0(machala0216$year, "-", machala0216$month, "-01")
machala0216$Date <- as.Date(machala0216$Date, "%Y-%m-%d")

# format dataset
machala0216$Site <- "Machala"
machala0216$denv_positive_clinically_diagnosed <- machala0216$cases
machala0216 <- machala0216[,c("Site", "Date", "denv_positive_clinically_diagnosed")]
machala0216 <- machala0216[complete.cases(machala0216),]

# combine dengue data -----------------------------------------------------------------------------------------
unconfirmed <- rbind(unconfirmed1418, cases0311)
unconfirmed <- unconfirmed[!(unconfirmed$Site == "Machala" & unconfirmed$Date <= max(machala0216$Date)), ]
unconfirmed <- rbind(unconfirmed, machala0216)
confirmed <- rbind(confirmed1416, cases1718)
dengue <- merge(unconfirmed, confirmed, by=c("Site", "Date"), all=T)
dengue$Year.Month <- substr(dengue$Date, 1, 7)

# Chikugunya --------------------------------------------------------------------------------------------------
chikv1518 <- read.csv("Ecuador/Case_data/CHIKV_El_Oro_cohort_cities_2015-2018.csv", head=T, stringsAsFactors = F)

# format dates
chikv1518$Fec.atencion <- as.Date(chikv1518$Fec.atencion, "%m/%d/%Y")
chikv1518$Date <- format(chikv1518$Fec.atencion, "%Y-%m-%d")

# summarize chikungunya cases 
chikv1518 <- ddply(chikv1518, .(Date, Canton..Domic), summarize, chikv_positive = sum(Casos[Confirmado.por=="Laboratorio"]), chikv_positive_clinically_diagnosed = sum(Casos[Confirmado.por!="Laboratorio"]))

# adjust chikungunya values based on Lowe et al. 2017 
chikv1518$Year.Month <- substr(chikv1518$Date, 1, 7)
chikv1518$chikv_positive_clinically_diagnosed <- ifelse(chikv1518$Year.Month == "2015-03", round(chikv1518$chikv_positive_clinically_diagnosed * 1.143), chikv1518$chikv_positive_clinically_diagnosed) 
chikv1518$chikv_positive_clinically_diagnosed <- ifelse(chikv1518$Year.Month == "2015-04", round(chikv1518$chikv_positive_clinically_diagnosed * 1.105), chikv1518$chikv_positive_clinically_diagnosed) 
chikv1518$chikv_positive_clinically_diagnosed <- ifelse(chikv1518$Year.Month == "2015-05", round(chikv1518$chikv_positive_clinically_diagnosed * 1.432), chikv1518$chikv_positive_clinically_diagnosed) 
chikv1518$chikv_positive_clinically_diagnosed <- ifelse(chikv1518$Year.Month == "2015-06", round(chikv1518$chikv_positive_clinically_diagnosed * 1.60), chikv1518$chikv_positive_clinically_diagnosed) 
chikv1518$chikv_positive_clinically_diagnosed <- ifelse(chikv1518$Year.Month == "2015-07", round(chikv1518$chikv_positive_clinically_diagnosed * 1.65), chikv1518$chikv_positive_clinically_diagnosed) 
chikv1518$chikv_positive_clinically_diagnosed <- ifelse(chikv1518$Year.Month == "2015-08", round(chikv1518$chikv_positive_clinically_diagnosed * 1.70), chikv1518$chikv_positive_clinically_diagnosed) 
chikv1518$chikv_positive_clinically_diagnosed <- ifelse(chikv1518$Year.Month == "2015-09", round(chikv1518$chikv_positive_clinically_diagnosed * 1.45), chikv1518$chikv_positive_clinically_diagnosed) 

# format sites
chikv1518$Site <-  paste0(substr(chikv1518$Canton..Domic, 1, 1), tolower(substr(chikv1518$Canton..Domic, 2, nchar(chikv1518$Canton..Domic))))
chikv1518$Canton..Domic <- NULL

# combine dengue and chikungunya data
cases <- merge(dengue, chikv1518, by=c("Site", "Date", "Year.Month"), all=T)

# aggregate data to monthly cases ----------------------------------------------------------------------------
cases <- ddply(cases, .(Site, Year.Month), summarize
               , denv_positive_clinically_diagnosed = ifelse(all(is.na(denv_positive_clinically_diagnosed))==TRUE, NA, sum(denv_positive_clinically_diagnosed, na.rm=T))
               , denv_positive = ifelse(all(is.na(denv_positive))==TRUE, NA, sum(denv_positive, na.rm=T))
               , chikv_positive = ifelse(all(is.na(chikv_positive))==TRUE, NA, sum(chikv_positive, na.rm=T))
               , chikv_positive_clinically_diagnosed = ifelse(all(is.na(chikv_positive_clinically_diagnosed))==TRUE, NA, sum(chikv_positive_clinically_diagnosed, na.rm=T))
               , Date = max(Date))

# save data
write.csv(cases, "Concatenated_Data/case_data/cases_by_month_Ecuador.csv", row.names = F)

# Zika data --------------------------------------------------------------------------------------------------
# could consider using: https://github.com/cdcepi/zika/tree/master/Ecuador/GACETA-ZIKA/data
zikv1618 <- read.csv("Ecuador/Case_data/ZIKV_El_Oro_cohort_cities_2016-2018.csv", head=T, stringsAsFactors = F)

# format dates
zikv1618$Fec.atencion <- as.Date(zikv1618$Fec.atencion, "%m/%d/%Y")
zikv1618$Date <- format(zikv1618$Fec.atencion, "%Y-%m-%d")
zikv1618$Year.Month <- substr(zikv1618$Date, 1, 7)

# remove non laboratory-confirmed Zika cases and subset to Machala (only one additional case in Huaquillas)
zikv1618 <- subset(zikv1618, Diagnostico.final == "ZIKA" & Confirmado.por == 'Laboratorio' & Canton..Domic == "MACHALA")

# summarize zika cases 
zikv1618 <- ddply(zikv1618, .(Year.Month, Canton..Domic), summarize, zikv_positive = sum(CASOS, na.rm=T), Date = max(Date))

# rename canton as site
colnames(zikv1618)[2] <- "Site"

# format site column
zikv1618$Site <- paste0(toupper(substr(zikv1618$Site, 1, 1)), tolower(substr(zikv1618$Site, 2, nchar(zikv1618$Site))))

# save
write.csv(zikv1618, "Concatenated_Data/case_data/Zika.csv", row.names = F)
