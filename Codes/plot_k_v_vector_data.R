# Plot carrying capacity function versus mosquito data -------------------
rm(list=ls()) #remove previous variable assignments
source("Codes/climate_dependent_k_functions.R")

# plot K functions ----------------------------------------------
# temperature ---------
tempSeq <- seq(12,40,.2)
temp <- c()
for (i in 1:length(tempSeq)){
  temp <- c(temp, K_temp(tempSeq[i]))
}
plot(tempSeq, temp, type='l', lwd=2, ylab=c("K (scaled)"), xlab=expression(paste("Temperature (",degree,"C)")), col="red4")

# humidity ------------
humSeq <- seq(0,100,1)
hum <- c()
for (i in 1:length(humSeq)){
  hum <- c(hum, K_hum(humSeq[i]))
}
plot(humSeq, hum, type='l', lwd=2, ylab=c("Mosquito carrying capacity (scaled)"), xlab=c("Relative humidity"), col="orange")

# rain ----------------
rainSeq <- seq(0,500,1)
r.briere <- c()
r.hump <- c()
r.linear <- c()
for (i in 1:length(rainSeq)){
  r.briere <- c(r.briere, K_rain_briere(rainSeq[i]))
  r.hump <- c(r.hump, K_rain_hump(rainSeq[i]))
  r.linear <- c(r.linear, K_rain_linear(rainSeq[i]))
}

plot(rainSeq, r.briere, type='l', lwd=2, ylab=c("Mosquito carrying capacity (scaled)"), xlab=c("Cumulative rainfall (mm)"), col="cadetblue")
plot(rainSeq, r.hump, type='l', lwd=2, ylab=c("Mosquito carrying capacity (scaled)"), xlab=c("Cumulative rainfall (mm)"), col="cadetblue")
plot(rainSeq, r.linear, type='l', lwd=2, ylab=c("Mosquito carrying capacity (scaled)"), xlab=c("Cumulative rainfall (mm)"), col="cadetblue")

# plot K functions v data ---------------------------------------
# K versus daily temps and humidity suitability values -----
climateData <- read.csv("Kenya/Concatenated_Data/climate/gapfilled_climate_data.csv", head=T)
climateData$Date <- as.Date(climateData$Date, "%Y-%m-%d")

# temperature
climateData$Chulaimbo_temp_suitability <- unlist(lapply(climateData$GF_Chulaimbo_mean_temp, K_temp))
climateData$Kisumu_temp_suitability <- unlist(lapply(climateData$GF_Kisumu_mean_temp, K_temp))
climateData$Msambweni_temp_suitability <- unlist(lapply(climateData$GF_Msambweni_mean_temp, K_temp))
climateData$Ukunda_temp_suitability <- unlist(lapply(climateData$GF_Ukunda_mean_temp, K_temp))

# humidity
climateData$Chulaimbo_humidity_suitability <- unlist(lapply(climateData$GF_Chulaimbo_humidity, K_hum))
climateData$Kisumu_humidity_suitability <- unlist(lapply(climateData$GF_Kisumu_humidity, K_hum))
climateData$Msambweni_humidity_suitability <- unlist(lapply(climateData$GF_Msambweni_humidity, K_hum))
climateData$Ukunda_humidity_suitability <- unlist(lapply(climateData$GF_Ukunda_humidity, K_hum))

# temperature
tiff("Kenya/Figures/K_functional_curves/temp_suit_v_site_temps.tiff", height = 900, width = 400)
par(mfrow=c(4,1))
plot(tempSeq, temp, type='l', lwd=2, ylab=c("K (scaled)"), xlab=expression(paste("Temperature (",degree,"C)")), col="red", main="Chulaimbo")
points(climateData$GF_Chulaimbo_mean_temp, climateData$Chulaimbo_temp_suitability)
plot(tempSeq, temp, type='l', lwd=2, ylab=c("K (scaled)"), xlab=expression(paste("Temperature (",degree,"C)")), col="red", main="Kisumu")
points(climateData$GF_Kisumu_mean_temp, climateData$Kisumu_temp_suitability)
plot(tempSeq, temp, type='l', lwd=2, ylab=c("K (scaled)"), xlab=expression(paste("Temperature (",degree,"C)")), col="red", main="Msambweni")
points(climateData$GF_Msambweni_mean_temp, climateData$Msambweni_temp_suitability)
plot(tempSeq, temp, type='l', lwd=2, ylab=c("K (scaled)"), xlab=expression(paste("Temperature (",degree,"C)")), col="red", main="Ukunda")
points(climateData$GF_Chulaimbo_mean_temp, climateData$Chulaimbo_temp_suitability)
dev.off()

# humidity
tiff("Kenya/Figures/K_functional_curves/hum_suit_v_hum_temps.tiff", height = 900, width = 400)
par(mfrow=c(4,1))
plot(humSeq, hum, type='l', lwd=2, ylab=c("K (scaled)"), xlab="Relative humidity", col="orange", main="Chulaimbo")
points(climateData$GF_Chulaimbo_humidity, climateData$Chulaimbo_humidity_suitability)
plot(humSeq, hum, type='l', lwd=2, ylab=c("K (scaled)"), xlab="Relative humidity", col="orange", main="Kisumu")
points(climateData$GF_Kisumu_humidity, climateData$Kisumu_humidity_suitability)
plot(humSeq, hum, type='l', lwd=2, ylab=c("K (scaled)"), xlab="Relative humidity", col="orange", main="Msambweni")
points(climateData$GF_Msambweni_humidity, climateData$Msambweni_humidity_suitability)
plot(humSeq, hum, type='l', lwd=2, ylab=c("K (scaled)"), xlab="Relative humidity", col="orange", main="Ukunda")
points(climateData$GF_Chulaimbo_humidity, climateData$Chulaimbo_humidity_suitability)
dev.off()

# K versus concatenated suitability values for temp, humidity, and rain -----
adults <- read.csv("Kenya/Concatenated_Data/vector/K_suitability_adults_house.csv", head=T)
larvae <- read.csv("Kenya/Concatenated_Data/vector/K_suitability_larvae_house.csv", head=T)
eggs <- read.csv("Kenya/Concatenated_Data/vector/K_suitability_eggs_house.csv", head=T)

# i levels
dfs <- list(adults, larvae, eggs) 
names <- c("adults", "larvae", "eggs")
# j levels
suitability <- c("T_suitability", "H_suitability", "R_suitability_briere", "R_suitability_hump", "R_suitability_linear")
yseq <- list(tempSeq, humSeq, rainSeq, rainSeq, rainSeq)
xseq <- list(temp, hum, r.briere, r.hump, r.linear)
xlabs <- c(expression(paste("Temperature (",degree,"C)")), "Relative humidity", rep("Cumulative rainfall (mm/month)", 3))
means <- c("Mean_temp", "Mean_humidity", rep("Cumulative_rain", 3))
colors <- c("red", "orange", rep("cadetblue", 3))
# k levels
sites <- c("Chulaimbo", "Kisumu", "Msambweni", "Ukunda")

for (i in 1:length(dfs)){
  df <- dfs[[i]]
  for (j in 1:length(suitability)){
    fileName <- paste0("Kenya/Figures/K_functional_curves/", suitability[j], "_v_data_", names[i], ".tiff")
    tiff(fileName, height = 900, width = 400)
    par(mfrow=c(4,1))
    for (k in 1:length(sites)){
      tempsitedf <- subset(df, study_site == sites[k])
      plot(yseq[[j]], xseq[[j]], type='l', lwd=2, ylab=c("K (scaled)"), xlab=xlabs[j], col=colors[j], main=sites[k])
      points(tempsitedf[,means[[j]]], tempsitedf[,suitability[[j]]])
    }
  dev.off()
  }
}

# boxplots ----------------------------------------------------------------------------
suitability <- c("T_suitability", "H_suitability", "R_suitability_briere", "R_suitability_hump", "R_suitability_linear")

for (l in 1:length(suitability)){
  filename <- paste0("Kenya/Figures/K_functional_curves/", suitability[l], "_by_site.tiff")
  tiff(filename, width=781, height=497)
  boxplot(adults[,suitability[l]]~adults$study_site, col="lightblue", ylab=c("K suitability (adults)"), main= suitability[l])
  dev.off()
}

# plot correlations among suitability metrics -----------------------------------------
source("C:/Users/Jamie/Box Sync/R_functions/correlation_pairs_fn.R")

correggs <- eggs[,c("T_suitability", "H_suitability", "R_suitability_briere", "R_suitability_hump", "R_suitability_linear")]
correggs[,] <- lapply(correggs[,], as.numeric)
tiff(file = "Kenya/Figures/K_functional_curves/corr_plot_THR.tiff", width = 872, height = 541)
pairs(correggs, lower.panel=panel.smooth, upper.panel=panel.cor,diag.panel=panel.hist)
dev.off()

# corr.egg <- eggs[,c("egg_total", "T_suitability", "H_suitability", "R_suitability_briere", "R_suitability_hump", "R_suitability_linear")]
# corr.egg[,] <- lapply(corr.egg[,], as.numeric)
# # tiff(file = "Kenya/Figures/K_functional_curves/corr_plot_THR.tiff", width = 872, height = 541)
# pairs(corr.egg, lower.panel=panel.smooth, upper.panel=panel.cor,diag.panel=panel.hist)
# # dev.off()
