# merge vector data --------------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load vector data
adults_kenya <- read.csv("Concatenated_Data/vector_data/Kenya_prokopack.csv", head=T, stringsAsFactors = F)
adults_ecuador <- read.csv("Concatenated_Data/vector_data/vector_data_Ecuador.csv", head=T, stringsAsFactors = F)
larvae <- read.csv("Concatenated_Data/vector_data/Kenya_larvae.csv", head=T, stringsAsFactors = F)
eggs <- read.csv("Concatenated_Data/vector_data/Kenya_ovitrap.csv", head=T, stringsAsFactors = F)

# combine vector data
adults_ecuador$Year.Month <- substr(adults_ecuador$Date, 1,7)
vectors <- rbind(adults_kenya[,c("Site", "Date", "Year.Month", "aedes_total")], adults_ecuador[,c("Site", "Date",  "Year.Month","aedes_total")])
vectors <- merge(vectors, larvae, by=c("Site", "Date",  "Year.Month"), all=T)
vectors <- merge(vectors, eggs, by=c("Site", "Date", "Year.Month"), all=T)

# add "weekly" headings
colnames(vectors)[4:8] <- paste0(colnames(vectors)[4:8], "_weekly")

# calculate monthly estimates
monthlyVectors <- ddply(vectors, .(Site, Year.Month)
                      , summarize
                      , Date = min(Date)
                      , aedes_total_monthly = ifelse(sum(is.na(aedes_total_weekly))==length(aedes_total_weekly), NA, sum(aedes_total_weekly, na.rm=T))
                      , pupae_total_monthly = sum(pupae_total_weekly, na.rm=T)
                      , early_instar_total_monthly = sum(early_instar_total_weekly, na.rm=T)
                      , late_instar_total_monthly = sum(late_instar_total_weekly, na.rm=T)
                      , egg_total_monthly = sum(egg_total_weekly, na.rm=T)
                      )

# merge weekly and monthly data
vectors <- merge(vectors, monthlyVectors, by=c("Site", "Date", "Year.Month"), all=T)
vectors$Year.Month <- NULL

# save data
write.csv(vectors, "Concatenated_Data/vector_data/merged_vector_data.csv", row.names = F)
