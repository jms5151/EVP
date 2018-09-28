# ----- Test for correlation and synchrony among vector data 
rm(list=ls()) #remove previous variable assignments
library(corrplot)
library(plyr)

#--- read in vector data and add prefixes to column names
bg <- read.csv("Concatenated_Data/vector/bg.csv", head=T, as.is=T)
colnames(bg)[5:14] <- paste("BG", colnames(bg)[5:14], sep = "_")

hlc <- read.csv("Concatenated_Data/vector/hlc.csv", head=T, as.is=T)
colnames(hlc)[6:15] <- paste("HLC", colnames(hlc)[6:15], sep = "_")

larvae <- read.csv("Concatenated_Data/vector/larvae.csv", head=T, as.is=T)

ovitrap <- read.csv("Concatenated_Data/vector/ovitrap.csv", head=T, as.is=T)

prokopak <- read.csv("Concatenated_Data/vector/prokopak.csv", head=T, as.is=T)
colnames(prokopak)[3:12] <- paste("Prokopak", colnames(prokopak)[3:12], sep = "_")

# https://www.oreilly.com/learning/a-new-visualization-to-beautifully-explore-correlations
#-------- correlations
#--- correlation within houses, sites, and survey times
vectorData <- merge(bg, hlc, by=c("redcap_event_name", "study_site", "mon.yr"), all=T)
vectorData <- merge(vectorData, larvae, by=c("redcap_event_name", "study_site", "mon.yr"), all=T)
vectorData <- merge(vectorData, ovitrap, by=c("redcap_event_name", "study_site", "mon.yr"), all=T)
vectorData <- merge(vectorData, prokopak, by=c("redcap_event_name", "study_site", "mon.yr"), all=T)

#mydata <- vectorData[,c(7:9,11,12,14,15,22:24,26,27,29,30,33:35,39,41:43,45,46,48,49)]
mydata <- vectorData[,c(14,15,29,30,33:35,39,48,49)]
# mydata <- vectorData[,c(15,30,33:35,39,49)] 
mydata1<-na.omit(mydata)
cormat <- round(cor(mydata1),2)
pdf("Figures/vectors/corrplot_houses_sites_and_times.pdf", width=11, height=8.5)
corrplot(cormat, method="circle", addCoef.col = "green", type="lower", tl.col="black", tl.srt=45, tl.cex = .9)
dev.off()

#--- correlation within sites and survey times
vectorDataTime <- merge(bg[,c(5:7,9,10,12,13,16,17)], hlc[,c(6:8,10,11,13,14,16,17)], by=c("study_site", "mon.yr"), all=T)
vectorDataTime <- merge(vectorDataTime, larvae[,c(3:7)], by=c("study_site", "mon.yr"), all=T)
vectorDataTime <- merge(vectorDataTime, ovitrap[,c(5:7)], by=c("study_site", "mon.yr"), all=T)
vectorDataTime <- merge(vectorDataTime, prokopak[,c(3:14)], by=c("study_site", "mon.yr"), all=T)

mydata <-  vectorDataTime[,c(8,9,15,16:20,28,29)] #vectorDataTime[,c(3:30)]
mydata2<-na.omit(mydata)
cormat <- round(cor(mydata2),2)
pdf("Figures/vectors/corrplot_sites_and_times.pdf", width=11, height=8.5)
corrplot(cormat, method="circle", addCoef.col = "green", type="lower", tl.col="black", tl.srt=45, tl.cex = .9)
dev.off()

#--- correlation within survey times
vecDataTimeOnly <- ddply(vectorDataTime, .(mon.yr),
                         summarise,
                         bgTotal = sum(na.omit(BG_a.aegypti_total)),
                         hlcTotal = sum(na.omit(HLC_a.aegypti_total)),
                         prokopakTotal = sum(na.omit(Prokopak_a.aegypti_total)),
                         eggs = sum(na.omit(eggs)),
                         early_instars = sum(na.omit(early_instars)),
                         late_instars = sum(na.omit(late_instars)),
                         pupae = sum(na.omit(pupae)))

mydata3 <- vecDataTimeOnly[,c(2:8)]
cormat <- round(cor(mydata3),2)
pdf("Figures/vectors/corrplot_times.pdf", width=11, height=8.5)
corrplot(cormat, method="circle", addCoef.col = "green", type="lower", tl.col="black", tl.srt=45, tl.cex = .9)
dev.off()

#------ correlation and synchrony among vector and model data
bg <- read.csv("Concatenated_Data/BG_Regrssion_Data.csv", head=T, as.is=T)
hlc <- read.csv("Concatenated_Data/HLC_Regrssion_Data.csv", head=T, as.is=T)
larvae <- read.csv("Concatenated_Data/Larvae_Regrssion_Data.csv", head=T, as.is=T)
ovitrap <- read.csv("Concatenated_Data/Ovitrap_Regrssion_Data.csv", head=T, as.is=T)
prokopak <- read.csv("Concatenated_Data/Prokopak_Regrssion_Data.csv", head=T, as.is=T)

library(synchrony)

#-- bg
timespans <- c("weekly.model.Total", "monthly.model.Total")
m.metrics <- c("a.aegypti_male", "a.aegypti_unfed", "a.aegypti_bloodfed", "a.aegypti_halfgravid",
               "a.aegypti_gravid", "a.aegypti_female", "a.aegypti_total")

df <- data.frame(peak=as.character(NA), correlation=as.character(NA), timespan=as.character(NA), mosquito_metric=as.character(NA), stringsAsFactors = F)

for (mosquito.variable in m.metrics){
  for (time.variable in timespans){
    pk <- tryCatch(peaks(bg[, time.variable], bg[,mosquito.variable]), error=function(err) NA)
    pk2 <- round(pk[[1]], 2)
    cor <- cor.test(bg[, time.variable], bg[,mosquito.variable])
    cor2 <- round(cor$estimate, 2)
    df2 <- c(pk2, cor2, time.variable, mosquito.variable)
    df <- rbind(df, df2)
  }
}

df$peak <- as.numeric(df$peak)
df$correlation <- as.numeric(df$correlation)

write.csv(df, "Concatenated_Data/vector/correlation_and_synchrony/BG_corr_peaks.csv", row.names = F)

#-- hlc
df <- data.frame(peak=as.character(NA), correlation=as.character(NA), timespan=as.character(NA), mosquito_metric=as.character(NA), stringsAsFactors = F)

for (mosquito.variable in m.metrics){
  for (time.variable in timespans){
    pk <- tryCatch(peaks(hlc[, time.variable], hlc[,mosquito.variable]), error=function(err) NA)
    pk2 <- round(pk[[1]], 2)
    cor <- cor.test(hlc[, time.variable], hlc[,mosquito.variable])
    cor2 <- round(cor$estimate, 2)
    df2 <- c(pk2, cor2, time.variable, mosquito.variable)
    df <- rbind(df, df2)
  }
}

df$peak <- as.numeric(df$peak)
df$correlation <- as.numeric(df$correlation)

write.csv(df, "Concatenated_Data/vector/correlation_and_synchrony/HLC_corr_peaks.csv", row.names = F)

#-- prokopak
df <- data.frame(peak=as.character(NA), correlation=as.character(NA), timespan=as.character(NA), mosquito_metric=as.character(NA), stringsAsFactors = F)

for (mosquito.variable in m.metrics){
  for (time.variable in timespans){
    pk <- tryCatch(peaks(prokopak[, time.variable], prokopak[,mosquito.variable]), error=function(err) NA)
    pk2 <- round(pk[[1]], 2)
    cor <- cor.test(prokopak[, time.variable], prokopak[,mosquito.variable])
    cor2 <- round(cor$estimate, 2)
    df2 <- c(pk2, cor2, time.variable, mosquito.variable)
    df <- rbind(df, df2)
  }
}

df$peak <- as.numeric(df$peak)
df$correlation <- as.numeric(df$correlation)

write.csv(df, "Concatenated_Data/vector/correlation_and_synchrony/Prokopak_corr_peaks.csv", row.names = F)


#-- larvae
m.metrics <- c("early_instars", "late_instars", "pupae")

df <- data.frame(peak=as.character(NA), correlation=as.character(NA), timespan=as.character(NA), mosquito_metric=as.character(NA), stringsAsFactors = F)

for (mosquito.variable in m.metrics){
  for (time.variable in timespans){
    pk <- tryCatch(peaks(larvae[, time.variable], larvae[,mosquito.variable]), error=function(err) NA)
    pk2 <- round(pk[[1]], 2)
    cor <- cor.test(larvae[, time.variable], larvae[,mosquito.variable])
    cor2 <- round(cor$estimate, 2)
    df2 <- c(pk2, cor2, time.variable, mosquito.variable)
    df <- rbind(df, df2)
  }
}

df$peak <- as.numeric(df$peak)
df$correlation <- as.numeric(df$correlation)

write.csv(df, "Concatenated_Data/vector/correlation_and_synchrony/Larvae_corr_peaks.csv", row.names = F)

#-- ovitrap
m.metrics <- c("eggs")

df <- data.frame(peak=as.character(NA), correlation=as.character(NA), timespan=as.character(NA), mosquito_metric=as.character(NA), stringsAsFactors = F)

for (mosquito.variable in m.metrics){
  for (time.variable in timespans){
    pk <- tryCatch(peaks(ovitrap[, time.variable], ovitrap[,mosquito.variable]), error=function(err) NA)
    pk2 <- round(pk[[1]], 2)
    cor <- cor.test(ovitrap[, time.variable], ovitrap[,mosquito.variable])
    cor2 <- round(cor$estimate, 2)
    df2 <- c(pk2, cor2, time.variable, mosquito.variable)
    df <- rbind(df, df2)
  }
}

df$peak <- as.numeric(df$peak)
df$correlation <- as.numeric(df$correlation)

write.csv(df, "Concatenated_Data/vector/correlation_and_synchrony/Ovitrap_corr_peaks.csv", row.names = F)

hist(df$peak)
hist(df$correlation)




sync.mins = phase.sync(vecDataTimeOnly$eggs, vecDataTimeOnly$prokopakTotal, mins=TRUE)
## Compute and interpolate phases using successive local maxima
sync.maxs=phase.sync(vecDataTimeOnly$eggs, vecDataTimeOnly$prokopakTotal)
## Plot distribution of phase difference
hist(sync.mins$deltaphase$mod_phase_diff_2pi)
phase.sync(vecDataTimeOnly$pupae, vecDataTimeOnly$late_instars)
kendall.w(vecDataTimeOnly$pupae, vecDataTimeOnly$late_instars)
peaks(na.omit(mydata))
