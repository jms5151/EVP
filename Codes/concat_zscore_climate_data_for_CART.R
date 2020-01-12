# concat z-scores with climate data for CART models --------------------------------------
rm(list=ls()) #remove previous variable assignments

# load data
load("Concatenated_Data/model_v_data_cases.RData")
load("Concatenated_Data/model_v_data_aedes.RData")

# functions
zscore <- function(x){(x - mean(x,na.rm=T)) / sd(x,na.rm=T)}

# characterize correspondance based on z-scores for model and data ----------------------
sites <- unique(cases_and_mods$Site)
zscoreDF_dengue <- data.frame()
zscoreDF_aedes <- data.frame()

for (siteX in sites){
  zscoreAedes <- subset(vectors_and_mods, Site == siteX)
  zscoreAedes[,c("zscore_aedes_total", "zscore_Mtot")] <- lapply(zscoreAedes[,c("aedes_total", "Mtot")], zscore)
  if (siteX == "Chulaimbo" | siteX == "Kisumu" | siteX == "Msambweni"| siteX == "Ukunda"){
    zscoreAedes[,c("zscore_pupae_total", "zscore_early_instar_total", "zscore_late_instar_total", "zscore_egg_total_adjusted")] <- lapply(zscoreAedes[,c("pupae_total", "early_instar_total", "late_instar_total", "egg_total_adjusted")], zscore)
  } else {
    zscoreAedes[,c("zscore_pupae_total", "zscore_early_instar_total", "zscore_late_instar_total", "zscore_egg_total_adjusted")] <- NA
  }
  zscoreDF_aedes <- rbind(zscoreDF_aedes, zscoreAedes)
  zscoreCases <- subset(cases_and_mods, Site == siteX)
  if (siteX == "Zaruma"){
    zscoreCases[,c("zscore_denv_positive", "zscore_chikv_positive", "zscore_I")] <- zscoreCases[,c("denv_positive", "chikv_positive", "I")]
  } else {
    zscoreCases[,c("zscore_denv_positive", "zscore_chikv_positive", "zscore_I")] <- lapply(zscoreCases[,c("denv_positive", "chikv_positive", "I")], zscore)
  } 
  if (siteX == "Machala"){
    zscoreCases$zscore_zikv_positive <- zscore(zscoreCases$zikv_positive)
  } else {
    zscoreCases$zscore_zikv_positive <- NA
  }
  zscoreDF_dengue <- rbind(zscoreDF_dengue, zscoreCases)
}

# correlations 
zscoreDF_aedesK <- subset(zscoreDF_aedes, Site == "Chulaimbo"|Site=="Kisumu"|Site=="Msambweni"|Site=="Ukunda")
cor.test(zscoreDF_aedesK$zscore_aedes_total, zscoreDF_aedesK$zscore_Mtot)
cor.test(zscoreDF_aedesK$zscore_pupae_total, zscoreDF_aedesK$zscore_Mtot)
cor.test(zscoreDF_aedesK$zscore_late_instar_total, zscoreDF_aedesK$zscore_Mtot)
cor.test(zscoreDF_aedesK$zscore_early_instar_total, zscoreDF_aedesK$zscore_Mtot)
cor.test(zscoreDF_aedesK$zscore_egg_total_adjusted, zscoreDF_aedesK$zscore_Mtot)

cor.test(zscoreDF_dengue$zscore_denv_positive, zscoreDF_dengue$zscore_I)
cor.test(zscoreDF_dengue$zscore_chikv_positive, zscoreDF_dengue$zscore_I)
cor.test(zscoreDF_dengue$zscore_zikv_positive, zscoreDF_dengue$zscore_I)

# plot zscores --------------------------------------------------------------------------
pdf("Figures/models_and_data/zscore_scatterplots.pdf", width=9, height=4)
# 1 row by 2 column plot
par(mfrow=c(1,2))
# plot vectors
par(mar = c(4.2, 4.1, 4.1, 0.5))
plot(zscoreDF_aedes$zscore_aedes_total, zscoreDF_aedes$zscore_Mtot, pch=21, bg='deepskyblue4', col='black', ylab='', xlab='', yaxt='n', xlim=c(-2,6), ylim=c(-2,6))
axis(side=2, las=2)
title(ylab="Predicted mosquitoes (z-score)", line=2.5)
title(xlab="Observed mosquitoes (z-score)", line=2)
abline(lm(zscoreDF_aedes$zscore_Mtot~zscoreDF_aedes$zscore_aedes_total), lwd=2)
corAedes <- cor.test(zscoreDF_aedes$zscore_aedes_total, zscoreDF_aedes$zscore_Mtot)
legend("topright", legend=c(paste0("r=", round(unname(corAedes$estimate),2)), paste0("N=", corAedes$parameter+2)), bty='n')
title(expression(paste("a. ", italic("Aedes aegpyti"))), adj=0, line=0.6)

# plot cases
par(mar = c(4.2, 4.1, 4.1, 1))
plot(zscoreDF_dengue$zscore_denv_positive, zscoreDF_dengue$zscore_I, pch=21, bg='deepskyblue4', col='black', ylab='', xlab='', yaxt='n', ylim=c(-1.5,5.5))
points(zscoreDF_dengue$zscore_chikv_positive, zscoreDF_dengue$zscore_I, pch=22, bg='darkgoldenrod1', col='black', ylab='', xlab='', yaxt='n', ylim=c(-1.5,5.5))
points(zscoreDF_dengue$zscore_zikv_positive, zscoreDF_dengue$zscore_I, pch=24, bg='darkred', col='black', ylab='', xlab='', yaxt='n', ylim=c(-1.5,5.5))
axis(side=2, las=2)
title(ylab="Predicted cases (z-score)", line=2.5)
title(xlab="Lab-confirmed cases (z-score)", line=2)
abline(lm(zscoreDF_dengue$zscore_I~zscoreDF_dengue$zscore_denv_positive), lwd=2)
corDengue <- cor.test(zscoreDF_dengue$zscore_denv_positive, zscoreDF_dengue$zscore_I)
legend(x=-0.7, y=5.7, legend=c("Dengue", "Chikungunya", "Zika"), text.col=c('deepskyblue4', 'darkgoldenrod1', 'darkred')
       , pch=c(21,22,24), pt.bg=c('deepskyblue4', 'darkgoldenrod1', 'darkred'), col=c('black', 'black', 'black')
       , bg='white', box.col='white')
legend("topright", legend=c(paste0("r=", round(unname(corDengue$estimate),2)), "N=388"), bty='n')
title(expression("b. Arboviruses"), adj=0, line=0.6)
# close plotting device
dev.off()

# abline(0,1, lwd=2) # y=x
# abline(1,1, lty=2) # +1SD
# abline(-1,1, lty=2) # -1SD

# categorize by correspondance ------------------------------------------------------------
zscoreDF_aedes$Adult_correspondence_magnitude <- ifelse(zscoreDF_aedes$zscore_aedes_total-zscoreDF_aedes$zscore_Mtot < -1, "Overprediction", NA)
zscoreDF_aedes$Adult_correspondence_magnitude <- ifelse(zscoreDF_aedes$zscore_aedes_total-zscoreDF_aedes$zscore_Mtot > 1, "Underprediction", zscoreDF_aedes$Adult_correspondence_magnitude)
zscoreDF_aedes$Adult_correspondence_magnitude <- ifelse(zscoreDF_aedes$zscore_aedes_total-zscoreDF_aedes$zscore_Mtot >= -1 & zscoreDF_aedes$zscore_aedes_total-zscoreDF_aedes$zscore_Mtot <= 1, "Correspondence", zscoreDF_aedes$Adult_correspondence_magnitude)

# add chikv and zikv surveys in if NA for dengue
zscoreDF_dengue$zscore_denv_positive <- ifelse(is.na(zscoreDF_dengue$zscore_denv_positive), zscoreDF_dengue$zscore_chikv_positive, zscoreDF_dengue$zscore_denv_positive)
zscoreDF_dengue$zscore_denv_positive <- ifelse(is.na(zscoreDF_dengue$zscore_denv_positive), zscoreDF_dengue$zscore_zikv_positive, zscoreDF_dengue$zscore_denv_positive)

zscoreDF_dengue$Dengue_correspondence_magnitude <- ifelse(zscoreDF_dengue$zscore_denv_positive-zscoreDF_dengue$zscore_I < -1, "Overprediction", NA)
zscoreDF_dengue$Dengue_correspondence_magnitude <- ifelse(zscoreDF_dengue$zscore_denv_positive-zscoreDF_dengue$zscore_I > 1, "Underprediction", zscoreDF_dengue$Dengue_correspondence_magnitude)
zscoreDF_dengue$Dengue_correspondence_magnitude <- ifelse(zscoreDF_dengue$zscore_denv_positive-zscoreDF_dengue$zscore_I >= -1 & zscoreDF_dengue$zscore_denv_positive-zscoreDF_dengue$zscore_I <= 1, "Correspondence", zscoreDF_dengue$Dengue_correspondence_magnitude)

# summarize results
table(zscoreDF_aedes$Adult_correspondence_magnitude)
table(zscoreDF_dengue$Dengue_correspondence_magnitude)

table(zscoreDF_aedes$Site, zscoreDF_aedes$Adult_correspondence_magnitude)
table(zscoreDF_dengue$Site, zscoreDF_dengue$Dengue_correspondence_magnitude)

# subset zscore data
zscoreSub_aedes <- zscoreDF_aedes[,c("Site", "Date", "Adult_correspondence_magnitude")]
zscoreSub_aedes <- subset(zscoreSub_aedes, !is.na(Adult_correspondence_magnitude))

zscoreSub_dengue <- zscoreDF_dengue[,c("Site", "Date", "Dengue_correspondence_magnitude")]
zscoreSub_dengue <- subset(zscoreSub_dengue, !is.na(Dengue_correspondence_magnitude))

# calculate climate conditions preceeding each observation ----------------------------------
load("Concatenated_Data/Climate_data/merged_climate_data.RData")

vars <- list(b = c("min", "mean", "max", "var"), c = c("T", "H", "R"))
climVars <- data.frame(do.call(expand.grid, vars))
clim_vars <- c(paste(climVars$c, climVars$b, sep="_"))#, "R_cum", "R_sum_days_1", "R_sum_days_3", "R_sum_days_5", "R_sum_days_10")
zscoreSub_aedes <- cbind(zscoreSub_aedes, setNames(lapply(clim_vars, function(x) x=NA), clim_vars) )
zscoreSub_dengue <- cbind(zscoreSub_dengue, setNames(lapply(clim_vars, function(x) x=NA), clim_vars) )

clim <- c("Temperature", "SVPD", "Two_week_rainfall")
clim2 <- c("T", "H", "R")  

# for aedes 
for (i in 1:nrow(zscoreSub_aedes)){
  clim.dat <- subset(climateData, Site == zscoreSub_aedes$Site[i])
  climRowIndexLast <- which(zscoreSub_aedes$Date[i] == clim.dat$Date) 
  climRowIndexFirst <- climRowIndexLast - 29
  for (j in 1:length(clim)){
    zscoreSub_aedes[i, paste0(clim2[j], "_min")] <- min(clim.dat[climRowIndexFirst:climRowIndexLast, clim[j]])
    zscoreSub_aedes[i, paste0(clim2[j], "_mean")] <- mean(clim.dat[climRowIndexFirst:climRowIndexLast, clim[j]])
    zscoreSub_aedes[i, paste0(clim2[j], "_max")] <- max(clim.dat[climRowIndexFirst:climRowIndexLast, clim[j]])
    zscoreSub_aedes[i, paste0(clim2[j], "_var")] <- var(clim.dat[climRowIndexFirst:climRowIndexLast, clim[j]])
  }
}

# for dengue 
for (i in 1:nrow(zscoreSub_dengue)){
  clim.dat <- subset(climateData, Site == zscoreSub_dengue$Site[i])
  climRowIndexLast <- which(zscoreSub_dengue$Date[i] == clim.dat$Date) 
  climRowIndexFirst <- climRowIndexLast - 29
  for (j in 1:length(clim)){
    zscoreSub_dengue[i, paste0(clim2[j], "_min")] <- min(clim.dat[climRowIndexFirst:climRowIndexLast, clim[j]])
    zscoreSub_dengue[i, paste0(clim2[j], "_mean")] <- mean(clim.dat[climRowIndexFirst:climRowIndexLast, clim[j]])
    zscoreSub_dengue[i, paste0(clim2[j], "_max")] <- max(clim.dat[climRowIndexFirst:climRowIndexLast, clim[j]])
    zscoreSub_dengue[i, paste0(clim2[j], "_var")] <- var(clim.dat[climRowIndexFirst:climRowIndexLast, clim[j]])
  }
}

# save data
save(zscoreSub_aedes,  file="Concatenated_Data/zscores_with_climate_data_aedes.RData")
save(zscoreSub_dengue,  file="Concatenated_Data/zscores_with_climate_data_dengue.RData")
