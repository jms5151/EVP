# calculate r, percentiles, and z-scores to assess correspondance between models and data
rm(list=ls()) #remove previous variable assignments

# load libraries
library(plyr)
library(Metrics)

# functions
# rsq <- function (x, y) cor(x, y) ^ 2
zscore <- function(x){(x - mean(x,na.rm=T)) / sd(x,na.rm=T)}
tau.calc <- function(x, y) {
  t <- cor.test(x, y, method="kendall")
  unname(t$estimate)
}
R2 <- function(t){(sin(0.5*pi*t)^2)}

# load data
load("Concatenated_Data/model_v_data.RData")

# calculate zscores
scores <- merged_data[grepl("I|Mtot|total|positive", colnames(merged_data))]
scores <- lapply(scores, zscore)
scores <- as.data.frame(scores)
colnames(scores) <- paste0(colnames(scores), "_zscore")
merged_data <- cbind(merged_data, scores)

# categorize by correspondance
merged_data$Adult_correspondence_magnitude <- ifelse(merged_data$aedes_total_zscore-merged_data$Mtot_zscore < -1, "Overprediction", NA)
merged_data$Adult_correspondence_magnitude <- ifelse(merged_data$aedes_total_zscore-merged_data$Mtot_zscore > 1, "Underprediction", merged_data$Adult_correspondence_magnitude)
merged_data$Adult_correspondence_magnitude <- ifelse(merged_data$aedes_total_zscore-merged_data$Mtot_zscore >= -1 & merged_data$aedes_total_zscore-merged_data$Mtot_zscore <= 1, "Correspondence", merged_data$Adult_correspondence_magnitude)

merged_data$Dengue_correspondence_magnitude <- ifelse(merged_data$denv_positive_zscore-merged_data$I_zscore < -1, "Overprediction", NA)
merged_data$Dengue_correspondence_magnitude <- ifelse(merged_data$denv_positive_zscore-merged_data$I_zscore > 1, "Underprediction", merged_data$Dengue_correspondence_magnitude)
merged_data$Dengue_correspondence_magnitude <- ifelse(merged_data$denv_positive_zscore-merged_data$I_zscore >= -1 & merged_data$denv_positive_zscore-merged_data$I_zscore <= 1, "Correspondence", merged_data$Dengue_correspondence_magnitude)

# table(merged_data$Adult_correspondence_magnitude)
# table(merged_data$Dengue_correspondence_magnitude)

# correspondance in time
# simobs$Year <- substr(simobs$Date, 1, 4)
# peakDengue <- ddply(simobs, .(Site, Year), summarize, peakDenvDate = Date[which.max(denv_positive_weekly)])
# peakMod <- ddply(simobs, .(Site, Year), summarize, peakIDate = Date[which.max(I_THR)])
# peaks <- merge(peakDengue, peakMod, by=c("Site", "Year"), all.x=T)
# peaks$PeakDiff <- as.Date(peaks$peakIDate, "%Y-%m-%d") - as.Date(peaks$peakDenvDate, "%Y-%m-%d")

# save data
save(merged_data, file="Concatenated_Data/model_v_data_zscores.RData")

# calculate correlation by region 
corr.region <- data.frame(matrix(ncol=5, nrow=0))
colnames(corr.region) <- c("Region", "Yvar", "RMSE", "tau", "R2")
regionnames <- c("Kenya", "Ecuador")

for (m in 1:length(regionnames)){
  df <- subset(merged_data, country == regionnames[m])
  if (regionnames[m]=="Ecuador"){
    yVars <- c("denv_positive_zscore", "denv_positive_clinically_diagnosed_zscore", "chikv_positive_zscore", "chikv_positive_clinically_diagnosed_zscore", "aedes_total_zscore")
  } else {
    yVars <- c("denv_positive_zscore", "chikv_positive_zscore", "aedes_total_zscore", "aedes_total_bg_zscore", "pupae_total_zscore", "late_instar_total_zscore", "early_instar_total_zscore", "egg_total_adjusted_zscore")
  }  
  for (n in 1:length(yVars)){
    firstLetter <- substr(yVars[n],1,1)
    if (firstLetter == "d"|firstLetter == "c"){
      modVar <- "I_zscore"
    } else {
      modVar <- "Mtot_zscore"
    }
    df2 <- df[,c(yVars[n], modVar)]
    df2 <- df2[complete.cases(df2),]
    rmse <- rmse(df2[,yVars[n]], df2[,modVar])
    tau <- tau.calc(df2[,yVars[n]], df2[,modVar])
    r2 <- R2(tau)  
    tmpvec <- c(regionnames[m], yVars[n], round(rmse, 2), round(tau, 2), round(r2, 2))
    corr.region[nrow(corr.region)+1,] <- tmpvec
  }
}

write.csv(corr.region, "Concatenated_Data/model_assessment/RMSE_tau_R2_by_region.csv", row.names=F)

# plot -------------------------------------------------------------------------------------------------------
# par(mfrow=c(3,3))
for (j in 1:length(regionnames)){
  if (regionnames[j]=="Ecuador"){
    yVars <- c("denv_positive_zscore", "denv_positive_clinically_diagnosed_zscore", "chikv_positive_zscore", "chikv_positive_clinically_diagnosed_zscore", "aedes_total_zscore")
    labels <- c("Dengue", "Dengue_CD", "Chikungunya", "Chikungunya_CD", "Aedes_aegypti")
    } else {
      yVars <- c("denv_positive_zscore", "chikv_positive_zscore", "aedes_total_zscore", "aedes_total_bg_zscore", "pupae_total_zscore", "late_instar_total_zscore", "early_instar_total_zscore", "egg_total_adjusted_zscore")
      labels <- c("Dengue", "Chikungunya", "Aedes_aegypti_prokopack", "Aedes_aegypti_bg", "Pupae", "Late instars", "Early instars", "Eggs")
    }
  for (k in 1:length(yVars)){
    if (substr(yVars[k], 1, 1) == "d" | substr(yVars[k], 1, 1) == "c"){
      xmod <- "I"
      } else {
        xmod <- "Mtot"
      }
    corr.all.index <- which(corr.region$Yvar == yVars[k] & corr.region$Region == regionnames[j])
    corr <- corr.region$tau[corr.all.index]
    r2 <- corr.region$R2[corr.all.index]
    rmse <- corr.region$RMSE[corr.all.index]
    fileName <- paste0("Figures/correlations/", regionnames[j], "_", yVars[k], ".tiff")
    tiff(fileName, width=900, height=650, res=150)
    plot(merged_data[,xmod], merged_data[,yVars[k]], cex=1.2, cex.lab = 1.2, cex.axis=1.2, pch=21, col="black", bg="deepskyblue4", xlab=paste0("Modeled ", labels[k]), ylab=paste0("Observed ", labels[k]))
    legend("bottomright", c(corr, r2, rmse), bty="n") 
    dev.off()
  }
}

# Plot correlations across specific models
# library(reshape2)
# library(ggplot2)
# corr.region <- read.csv("Concatenated_Data/model_assessment/RMSE_tau_R2_by_region.csv", head=T)
# corr.region <- corr.region[grepl("aedes_total_weekly_zscore|denv_positive_weekly_zscore", corr.region$Yvar),c("Region", "Yvar", "THR_tau", "T_tau", "H_tau", "R_tau")]
# corr.region <- melt(corr.region, id.vars = c("Region", "Yvar"))
# corr.region$response <- gsub("aedes_total_weekly_zscore", "Mosquitoes", corr.region$Yvar)
# corr.region$response <- gsub("denv_positive_weekly_zscore", "Dengue", corr.region$response)
# mosquitoes <- subset(corr.region, response == "Mosquitoes")
# dengue <- subset(corr.region, response == "Dengue")
# 
# ggplot(mosquitoes, aes(fill=variable, y=value, x=Region)) + 
#   geom_bar(position="dodge", stat="identity") + theme_classic() + 
#   geom_hline(yintercept=0) + scale_fill_manual(values=c("black", "darkred", "darkgoldenrod1", "deepskyblue4"))
# 
# ggplot(dengue, aes(fill=variable, y=value, x=Region)) +
#   geom_bar(position="dodge", stat="identity") + theme_classic() + 
#   geom_hline(yintercept=0) + scale_fill_manual(values=c("black", "darkred", "darkgoldenrod1", "deepskyblue4"))
