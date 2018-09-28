# compare model simulations with vector data using z-scores -----------------------------------
rm(list=ls()) #remove previous variable assignments

# load data ---------------------------------------------------------------
trait.df <- read.csv("Kenya/Concatenated_Data/SEI-SEIR/SEI-SEIR_simulations_with_trait_variation.csv", head=T)
adults.site <- read.csv("Kenya/Concatenated_Data/vector/K_suitability_adults_site_lags.csv", head=T, stringsAsFactors = F)
# larvae.site <- read.csv("Kenya/Concatenated_Data/vector/K_suitability_larvae_site_lags.csv", head=T, stringsAsFactors = F)
# eggs.site <- read.csv("Kenya/Concatenated_Data/vector/K_suitability_eggs_site_lags.csv", head=T, stringsAsFactors = F)

# calculate total mosquitoes
trait.df$Mtot <- rowSums(trait.df[,c("M1", "M2", "M3")])

# calculate z-score values ------------------------------------------------
z.dfs <- list(trait.df, adults.site) #, larvae.site, eggs.site)
dfNames <- c("trait.df", "adults.site") #, "larvae.site", "eggs.site")
yvars <- c("Mtot", "aedes_total") #, "late_instar_total", "egg_total")

for (i in 1:length(z.dfs)){
  tempZdf <- z.dfs[[i]]
  if (dfNames[i] == "trait.df"){
    tempZdf$Date <- as.Date(tempZdf$Date, "%Y-%m-%d")
  } else {
    tempZdf$Date <- as.Date(tempZdf$date_collected, "%Y-%m-%d")
  }
  tempZdf <- tempZdf[complete.cases(tempZdf),]
  pop_sd <- sd(tempZdf[,yvars[i]])*sqrt((length(tempZdf[,yvars[i]])-1)/(length(tempZdf[,yvars[i]])))
  pop_mean <- mean(tempZdf[,yvars[i]])
  tempZdf$zscore <- (tempZdf[,yvars[i]] - pop_mean) / pop_sd
  assign(dfNames[i], tempZdf)
}

# combine simulated and trap data by date and site -----------------------
trait.df$study_site <- trait.df$Site
trait.df$zscore_sim <- trait.df$zscore
adults.site$zscore_obs <- adults.site$zscore
zscoreDF <- merge(trait.df, adults.site, by=c("study_site", "Date"))

# correlation ------------------------------------------------------------
# cor.test(zscoreDF$zscore_sim, zscoreDF$zscore_obs)

# plot -------------------------------------------------------------------
# plot(zscoreDF$zscore_sim, zscoreDF$zscore_obs, pch=21, bg='lightblue', col='grey50', xlab=c('z-score (modeled mosquitoes)'), ylab=c('z-score (observed mosquitoes)'))
# abline(0,1, lwd=2)
# abline(-1,1, lty=2, lwd=2)
# abline(1,1, lty=2, lwd=2)

# difference & classification -------------------------------------------
zscoreDF$zscore.diff <- zscoreDF$zscore_obs - zscoreDF$zscore_sim
zscoreDF$class[zscoreDF$zscore.diff > 1] <- "Underprediction" #"blue"
zscoreDF$class[zscoreDF$zscore.diff < -1] <- "Overprediction" #"green" 
zscoreDF$class[zscoreDF$zscore.diff >= -1 & zscoreDF$zscore.diff <= 1] <- "Comparable" #"purple"
# plot(zscoreDF$zscore_sim, zscoreDF$zscore_obs, col=zscoreDF$class, xlab=c('z-score (modeled mosquitoes)'), ylab=c('z-score (observed mosquitoes)'), pch=16)

# save as csv ----------------------------------------------------------
zscoreDF2 <- zscoreDF[,c("study_site", "Date", "Mtot", "aedes_total", "Mean_temp", "Mean_humidity", "Cumulative_rain", "zscore.diff", "class")]
write.csv(zscoreDF2, "Kenya/Concatenated_Data/zscore_model_v_vectors.csv", row.names = F)