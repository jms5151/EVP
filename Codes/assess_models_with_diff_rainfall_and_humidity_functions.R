# assessing different rainfall functions in models with data ------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(Metrics)

# load case data
load("Concatenated_Data/case_data/merged_case_data.RData")
# add any arbovirus transmission??

# load vector data
load("Concatenated_Data/vector_data/merged_vector_data.RData")

# merge observed data
data <- merge(cases, vectors, by=c("Site", "Date", "country"), all=T)

# load modeled data with different rainfall functions
models <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_THR_test_diff_rain_functions.csv", head=T, stringsAsFactors = F)
models$Mtot <- models$M1 + models$M2 + models$M3
models$country <- ifelse(models$Site=="Chulaimbo"|models$Site=="Kisumu"|models$Site=="Msambweni"|models$Site=="Ukunda", "Kenya", "Ecuador")
models$model <- paste0(models$Rain_metric, "_", models$Rain_function, "-", models$Humidity_function)
models$Date <- as.Date(models$Date, "%Y-%m-%d")

# merge data
data <- merge(data, models, by=c("Site", "Date", "country"), all=T)

# calculate z-scores
# zscore <- function(x){(x - mean(x,na.rm=T)) / sd(x,na.rm=T)}
# scores <- data[grepl("I|Mtot|total|positive", colnames(data))]
# scores <- lapply(scores, zscore)
# scores <- as.data.frame(scores)
# colnames(scores) <- paste0(colnames(scores), "_zscore")
# data <- cbind(data, scores)

# set up dataframe
rmse_results <- data.frame(matrix(ncol=4, nrow=0))
colnames(rmse_results) <- c("Region", "Yvar", "RMSE", "model")

regionnames <- c("Kenya", "Ecuador")
mods <- unique(models$model)

for (m in 1:length(regionnames)){
  datasub <- subset(data, country == regionnames[m])
  if (regionnames[m]=="Ecuador"){
    yVars <- c("denv_positive", "denv_positive_clinically_diagnosed", "chikv_positive", "aedes_total")
  } else {
    yVars <- c("denv_positive", "chikv_positive", "aedes_total", "aedes_total_bg", "pupae_total", "late_instar_total", "early_instar_total", "egg_total_adjusted")
  }  
  for (n in 1:length(yVars)){
    firstLetter <- substr(yVars[n],1,1)
    if (firstLetter == "d"|firstLetter == "c"){
      modVar <- "I"
    } else {
      modVar <- "Mtot"
    }
    for (o in 1:length(mods)){
      tempDF <- subset(datasub, model == mods[o])
      tempDF <- tempDF[,c(yVars[n], modVar)]
      tempDF <- tempDF[complete.cases(tempDF),]
      if (nrow(tempDF) == 0){
        RMSE <- NA
      } else {
        RMSE <- round(rmse(tempDF[,yVars[n]], tempDF[,modVar]),2)
      }
      tmpvec <- c(regionnames[m], yVars[n], RMSE, mods[o])
      rmse_results[nrow(rmse_results)+1,] <- tmpvec
    }
  }
}

write.csv(rmse_results, "Concatenated_Data/model_assessment/rmse_diff_rain_and_hum_functions.csv", row.names=F)

rmse_results$R <- sapply(strsplit(rmse_results$model, "-|\\s"), "[", 1)
rmse_results$H <- paste0(sapply(strsplit(rmse_results$model, "-|\\s"), "[", 2), "_", sapply(strsplit(rmse_results$model, "-|\\s"), "[", 3))

# plot results
library(ggplot2)
colors <- colorRampPalette(c("white", "lightblue", "darkblue", "purple"))(42)

regions <- unique(rmse_results$Region)
yvars <- unique(rmse_results$Yvar)

for (Reg in regions){
  for (yv in yvars){
    df <- subset(rmse_results, Region == Reg & Yvar == yv)
    x <- ggplot(data = df, aes(x=R, y=H, fill=RMSE)) + 
      ggtitle(paste0(Reg, " ", yv)) +
      geom_tile() +
      geom_text(aes(label=RMSE)) +
      theme(legend.position = "none", axis.text.x = element_text(angle=30,hjust=1,vjust=1.0)) +
      xlab('') + ylab('') + 
      scale_fill_manual(values=colors)
    fname <- paste0("Figures/model_comparisons/", Reg, "_", yv, ".png")
    ggsave(fname)
  }
}


# add one legend indicating observations vs predictions
# add legend to each plot for number of model used, num data points, and correlation
