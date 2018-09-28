# vector house and site visit overview -------------------------------------
rm(list=ls()) #remove previous variable assignments
rm(list= ls()[!(ls() %in% c('clim_vec_backup','redcap_clim_vec'))])

library(plyr)
library(car)
library(MASS)
library(ggplot2)

# get data -----------------------------------------------------------------
# source("Codes/concat_vector_data_from_redcap.R")
bg <- read.csv("Kenya/Concatenated_Data/vector/redcap_bg.csv", head=T) 
hlc_sex <- read.csv("Kenya/Concatenated_Data/vector/redcap_hlc_sex.csv", head=T) 
hlc_time <- read.csv("Kenya/Concatenated_Data/vector/redcap_hlc_time.csv", head=T) 
larvae <- read.csv("Kenya/Concatenated_Data/vector/redcap_larvae.csv", head=T) 
ovitrap <- read.csv("Kenya/Concatenated_Data/vector/redcap_ovitrap.csv", head=T) 
prokopack <- read.csv("Kenya/Concatenated_Data/vector/redcap_prokopack.csv", head=T) 

# summarize data -----------------------------------------------------------
trapDF <- list(bg, hlc_sex, prokopack, larvae, ovitrap)
traps <- c("BG", "HLC", "Prokopack", "Larvae", "Ovitrap")

trapDataAllSites <- data.frame()
trapDataAllHouses <- data.frame()

# Create and save 1) Number of visits per house per site (per trap and for all traps)
#             and 2) Number of unique houses per site
for (i in 1:length(trapDF)){
  tabs <- ddply(trapDF[[i]], .(study_site, vector_house_id),
                summarise,
                numVisits = length(unique(date_collected)))
  tabs$study_site[is.na(tabs$study_site)]<-"Missing_site_info"
  tabs$trap <- traps[i]
  fileName = paste0("Kenya/Concatenated_Data/vector/summary_visits/", traps[i], "_house_visit_summary.csv")
  write.csv(tabs, fileName, row.names = F)
  trapDataAllHouses <- rbind(trapDataAllHouses, tabs)
  newdat <- as.data.frame(table(tabs$study_site))
  colnames(newdat) <- c("study_site", "unique_houses")
  newdat$trap <- traps[i]
  trapDataAllSites <- rbind(trapDataAllSites, newdat)
  assign(traps[i], tabs)
}

write.csv(trapDataAllSites, "Kenya/Concatenated_Data/vector/summary_visits/num_houses_per_site.csv", row.names = F)
write.csv(trapDataAllHouses, "Kenya/Concatenated_Data/vector/summary_visits/all_traps_house_visit_summary.csv", row.names = F)

# Create and save commonality among houses for different trap types 
trapDataAllHouses2 <- ddply(trapDataAllHouses, .(study_site, vector_house_id),
                            summarise,
                            num_shared_traps = length(unique(trap)),
                            shared_traps = list(unique(trap)))
trapDataAllHouses2$shared_traps <- unlist(as.character(trapDataAllHouses2$shared_traps))
write.csv(trapDataAllHouses2, "Kenya/Concatenated_Data/vector/summary_visits/shared_houses_per_trap.csv", row.names = F)

# plot commonality of shared trap types
trapDataAllHouses3 <- ddply(trapDataAllHouses2, .(study_site, num_shared_traps),
                            summarise,
                            num_houses = length(num_shared_traps))
trapDataAllHouses3$num_shared_traps <- as.factor(trapDataAllHouses3$num_shared_traps)

ggplot(data=trapDataAllHouses3, aes(x=study_site, y=num_houses, fill=num_shared_traps)) +
  geom_bar(stat="identity", position=position_dodge())+
  labs(title="Commonality among houses for different trap types", 
       x="Study site", y = "Number of houses") + scale_fill_brewer(palette="Blues")+ theme_minimal()+ guides(fill=guide_legend(title="Shared traps"))

# plot frequency of site visits -----------------------------------------------
trapDF <- list(bg, hlc_sex, prokopack, larvae, ovitrap)
traps <- c("BG", "HLC", "Prokopack", "Larvae", "Ovitrap")

for (i in 1:length(trapDF)){
  df <- trapDF[[i]]
  df$date_collected <- as.Date(df$date_collected, "%Y-%m-%d")
  df$yr.month <-format(df$date_collected, "%Y-%m")
  df$study_site <- as.factor(df$study_site)
  date_collected <- seq.Date(min(df$date_collected), max(df$date_collected), by="month")
  date_collected <- as.data.frame(date_collected)
  date_collected$yr.month <- format(date_collected$date_collected, "%Y-%m")
  df <- merge(date_collected, df, by=c("date_collected", "yr.month"), all=T)
  ggplot() + geom_point(data = df, aes(x = yr.month, y = study_site, color = study_site)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = traps[i]) +
    xlab("") + ylab("") 
  filename=paste0("Kenya/Figures/vectors/survey_frequency/Site_visit_frequency_",traps[i], ".tiff")
  ggsave(filename, width = 8, height = 4)
}

# plot frequency of house visits ----------------------------------------------
sites <- c("Chulaimbo", "Kisumu", "Msambweni", "Ukunda")

for (i in 1:length(trapDF)){
  df <- trapDF[[i]]
  df$date_collected <- as.Date(df$date_collected, "%Y-%m-%d")
  df$yr.month <-format(df$date_collected, "%Y-%m")
  df$study_site <- as.factor(df$study_site)
  df$vector_house_id <- as.factor(df$vector_house_id)
  date_collected <- seq.Date(min(df$date_collected), max(df$date_collected), by="month")
  date_collected <- as.data.frame(date_collected)
  date_collected$yr.month <- format(date_collected$date_collected, "%Y-%m")
  df <- merge(date_collected, df, by=c("date_collected", "yr.month"), all=T)
  for (j in 1:length(sites)){
    siteDF <- subset(df, study_site == sites[j]|study_site == j)
    ggplot() + geom_point(data = siteDF, aes(x = yr.month, y = vector_house_id)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = paste0(traps[i], " Site ", sites[j])) +
      xlab("") + ylab("") 
    filename=paste0("Kenya/Figures/vectors/survey_frequency/Visit_freq_",traps[i], "_site_", sites[j], ".tiff")
    ggsave(filename, width = 8, height = 4)
  }
}

# plot mean and se of aedes aegypti abundance through time --------------------
# se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

for (i in 1:length(trapDF)){
  df <- trapDF[[i]]
  df$date_collected <- as.Date(df$date_collected, "%Y-%m-%d")
  df$yr.month <-format(df$date_collected, "%Y-%m")
  df$study_site <- as.factor(df$study_site)
  date_collected <- seq.Date(min(df$date_collected), max(df$date_collected), by="month")
  date_collected <- as.data.frame(date_collected)
  date_collected$yr.month <- format(date_collected$date_collected, "%Y-%m")
  df <- merge(date_collected, df, by=c("date_collected", "yr.month"), all=T)
  if (traps[i] == "Larvae"){
    df$mosquitoes <- rowSums(df[,c("early_instars_larva", "late_instars_larva", "pupae_larva")], na.rm=T)
  } else if (traps[i] == "Ovitrap"){
    df$mosquitoes <- df$egg_count_ovitrap
  }
  df2 <- ddply(df, .(yr.month, study_site),
               summarise,
               mean_mosquitoes = mean(mosquitoes, na.rm = T))
  ggplot() + geom_point(data = df2, aes(x = yr.month, y = study_site, color = study_site, size=mean_mosquitoes)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = traps[i]) +
    xlab("") + ylab("") 
  filename=paste0("Kenya/Figures/vectors/survey_frequency/Abundance_by_site_",traps[i], ".tiff")
  ggsave(filename, width = 8, height = 4)
}
