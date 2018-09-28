# compare model simulations with case data using z-scores -----------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries ----------------------------------------------------------
library(plyr)

# load data ---------------------------------------------------------------
trait.df <- read.csv("Kenya/Concatenated_Data/SEI-SEIR/SEI-SEIR_simulations_with_trait_variation.csv", head=T)
aic_cases <- read.csv("Kenya/Concatenated_Data/aic_timeline.csv", head=T, stringsAsFactors = F)

# Monthly summation ------------------------------------------------------
aic_cases[,c("visit_a_int_date", "infected.t0", "infected.t1")] <- lapply(aic_cases[,c("visit_a_int_date", "infected.t0", "infected.t1")], as.Date)
aic_cases$mn.yr <- format(aic_cases$infected.t0, "%Y-%m")
aic_cases$Date <- paste0(aic_cases$mn.yr, "-01")
aic_cases$Date <- as.Date(aic_cases$Date, "%Y-%m-%d")

aic_cases_denv <- subset(aic_cases, visit_a_infected_denv == 1)
d2 <- ddply(aic_cases_denv, .(id_site, Date), summarize, denv_positive = length(unique(person_id)))

aic_cases_chikv <- subset(aic_cases, visit_a_infected_chikv == 1)
c2 <- ddply(aic_cases_chikv, .(id_site, Date), summarize, chikv_positive = length(unique(person_id)))

# calculate z-score values ------------------------------------------------
z.dfs <- list(trait.df, d2, c2) 
dfNames <- c("trait.df", "aic_cases_denv", "aic_cases_chikv") 
yvars <- c("I", "denv_positive", "chikv_positive") 

for (i in 1:length(z.dfs)){
  tempZdf <- z.dfs[[i]]
  tempZdf$Date <- as.Date(tempZdf$Date, "%Y-%m-%d")
  tempZdf <- tempZdf[complete.cases(tempZdf),]
  pop_sd <- sd(tempZdf[,yvars[i]])*sqrt((length(tempZdf[,yvars[i]])-1)/(length(tempZdf[,yvars[i]])))
  pop_mean <- mean(tempZdf[,yvars[i]])
  tempZdf$zscore <- (tempZdf[,yvars[i]] - pop_mean) / pop_sd
  colnames(tempZdf)[ncol(tempZdf)] <- paste0(yvars[i], "_zscore")
  assign(dfNames[i], tempZdf)
}

aic_positive_cases <- merge(aic_cases_denv, aic_cases_chikv, by=c("id_site", "Date"), all=T)
zscoreDF2 <- zscoreDF2[!duplicated(zscoreDF2), ]

# combine simulated and case data by date and site -----------------------
trait.df$id_site <- trait.df$Site
trait.df$zscore_sim <- trait.df$zscore
zscoreDF <- merge(trait.df, aic_positive_cases, by=c("id_site", "Date"))

# correlation ------------------------------------------------------------
# cor.test(zscoreDF$zscore_sim, zscoreDF$zscore_denv_obs)
# cor.test(zscoreDF$zscore_sim, zscoreDF$zscore_chikv_obs)

# plot -------------------------------------------------------------------
# plot(zscoreDF$zscore_sim, zscoreDF$zscore_denv_obs, pch=21, bg='lightblue', col='grey50', xlab=c('z-score (modeled cases)'), ylab=c('z-score (observed cases)'), ylim=c(-1,4))
# plot(zscoreDF$zscore_sim, zscoreDF$zscore_chikv_obs, pch=21, bg='lightblue', col='grey50', xlab=c('z-score (modeled cases)'), ylab=c('z-score (observed cases)'), ylim=c(-1,4))
# abline(0,1, lwd=2)
# abline(-1,1, lty=2, lwd=2)
# abline(1,1, lty=2, lwd=2)

# difference & classification -------------------------------------------
# dengue
zscoreDF$zscore.diff.denv <- zscoreDF$denv_positive_zscore - zscoreDF$zscore_sim
zscoreDF$denv_class[zscoreDF$zscore.diff.denv > 1] <- "Underprediction" #"blue"
zscoreDF$denv_class[zscoreDF$zscore.diff.denv < -1] <- "Overprediction" #"green" 
zscoreDF$denv_class[zscoreDF$zscore.diff.denv >= -1 & zscoreDF$zscore.diff.denv <= 1] <- "Comparable" #"purple"

# chikungunya
zscoreDF$zscore.diff.chikv <- zscoreDF$chikv_positive_zscore - zscoreDF$zscore_sim
zscoreDF$chikv_class[zscoreDF$zscore.diff.chikv > 1] <- "Underprediction" #"blue"
zscoreDF$chikv_class[zscoreDF$zscore.diff.chikv < -1] <- "Overprediction" #"green" 
zscoreDF$chikv_class[zscoreDF$zscore.diff.chikv >= -1 & zscoreDF$zscore.diff.chikv <= 1] <- "Comparable" #"purple"

# save as csv ----------------------------------------------------------
zscoreDF2 <- zscoreDF[,c("id_site", "Date", "chikv_positive", "denv_positive", "chikv_class", "denv_class")]
# zscoreDF2 <- zscoreDF2[!duplicated(zscoreDF2), ]
write.csv(zscoreDF2, "Kenya/Concatenated_Data/zscore_model_v_cases.csv", row.names = F)
