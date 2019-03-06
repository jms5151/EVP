rm(list=ls()) #remove previous variable assignments

# load libraries
library(Metrics)
library(purrr)

# load data
simobs <- read.csv("Concatenated_Data/model_v_data_zscores.csv", head = T)

# add country variable
simobs$Country <- ifelse(simobs$Site=="Chulaimbo"|simobs$Site=="Kisumu"|simobs$Site=="Msambweni"|simobs$Site=="Ukunda", "Kenya", "Ecuador")

# separate data by country
Kenya <- subset(simobs, Country=="Kenya" & !is.na(I_THR_zscore))
Ecuador <- subset(simobs, Country=="Ecuador" & !is.na(I_THR_zscore))

# create dataframes repeating modeled modeled mosquitoes and modeled disease cases x times
x <- 500
Kenya.Mosquitoes <- as.data.frame(replicate(x, Kenya$Mtot_THR_zscore))
Kenya.Cases <- as.data.frame(replicate(x, Kenya$I_THR_zscore))
Ecuador.Mosquitoes <- as.data.frame(replicate(x, Ecuador$Mtot_THR_zscore))
Ecuador.Cases <- as.data.frame(replicate(x, Ecuador$I_THR_zscore))

# calculate rmse for permutations for each dataset
modeledData <- list(Kenya.Mosquitoes, Kenya.Cases, Ecuador.Mosquitoes, Ecuador.Cases)
countries <- c(rep("Kenya", 2), rep("Ecuador", 2))
rmseResults <- data.frame(matrix(nrow=500, ncol=0))

for (i in 1:length(modeledData)){
  dat <- modeledData[[i]]
  if (countries[i]=="Ecuador"){
    yvars <- c("chikv_positive_weekly_zscore", "denv_positive_0311_weekly_zscore", "denv_positive_1418_weekly_zscore", "denv_positive_weekly_zscore", "denv_positive_weekly_any_zscore", "aedes_total_weekly_zscore")
    obsdat <- Ecuador
  } else {
    yvars <- c("chikv_positive_weekly_zscore", "denv_positive_weekly_zscore", "aedes_total_weekly_zscore", "pupae_total_weekly_zscore", "early_instar_total_weekly_zscore", "late_instar_total_weekly_zscore", "egg_total_weekly_zscore")
    obsdat <- Kenya
  }
  for (j in 1:length(yvars)){
    firstLetter <- substr(yvars[j], 1, 1)
    if (firstLetter == "d" | firstLetter == "c"){
      modvar <- "I_THR_zscore"
    } else {
      modvar <- "Mtot_THR_zscore"
    }
    dat[,] <- lapply(dat[,], function(x) sample(x))
    indexes <- which(!is.na(obsdat[,yvars[j]])==TRUE)
    obs <- obsdat[indexes, yvars[j]]
    mod <- dat[indexes,]
    rmse.results <- lapply(mod, function(x) rmse(obs, x)) # calculate rmse for each column
    rmse.results <- map_dbl(rmse.results, mean) # convert results into a vector of values
    rmseResults <- cbind(rmseResults, rmse.results)
    colnames(rmseResults)[ncol(rmseResults)] <- paste0(countries[i], ".", yvars[j])
  }
}

# save results
write.csv(rmseResults, "Concatenated_Data/model_assessment/rmse_permutations.csv", row.names=F)

# plot results -------------------------------------------------------------------------------------
rmseResults <- read.csv("Concatenated_Data/model_assessment/rmse_permutations.csv", head=T)
corr.region <- read.csv("Concatenated_Data/model_assessment/RMSE_tau_R2_by_region.csv", head=T)

for (k in 1:ncol(rmseResults)){
  region <- unlist(lapply(strsplit(colnames(rmseResults)[k], '.', fixed = TRUE), '[', 1))
  yvar <- unlist(lapply(strsplit(colnames(rmseResults)[k], '.', fixed = TRUE), '[', 2))
  index <- which(corr.region$Region==region & corr.region$Yvar==yvar)
  ttestval <- t.test(rmseResults[,k], mu=corr.region$THR_RMSE[index])
  pval <- round(unname(ttestval$p.value), 3)
  filename <- paste0("Figures/RMSE_permutations/", region, "_", yvar, "_RMSE.tiff")
  tiff(filename, width=782, height=541)
  par(yaxs="i") 
  hist(rmseResults[,k], col="lightgrey", main = region, xlim=c(0.49,2.18), xlab=yvar)
  abline(v=corr.region$THR_RMSE[index], lwd=2, lty=2, col='deepskyblue4')
  legend("topleft", paste0("p-value = ", pval), bty='n')
  dev.off()
}

