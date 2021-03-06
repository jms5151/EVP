# SEI-SEIR model simulations with immune heterogeneity -----------------------------------
rm(list=ls()) #remove previous variable assignments

# load packages
library(deSolve)

# load model
source("Codes/SEI-SEIR_model_THR.R")

# load climate data 
climateData <- read.csv("Concatenated_Data/climate_data/merged_climate_data.csv", head=T)
climateData$Date <- as.Date(climateData$Date, "%Y-%m-%d")

# set initial conditions 
inits <- read.csv("Concatenated_Data/sensitivity_analyses/LHS_inputs.csv", head=T)
startIC <- subset(inits, IC == "18")

# set different immigration and emigration rates to simulate over
ieRates<-c(0.06,0.07,0.08,0.09,0.10)

# set site conditions 
population <- c(7304, 547557, 240698, 154048, 57370, 279890, 13670, 25620)
sites <- c("Chulaimbo", "Kisumu", "Msambweni", "Ukunda", "Huaquillas", "Machala", "Portovelo", "Zaruma")
BRs <- c(rep(31.31,4),rep(20.18,4)) # birth rates from https://data.worldbank.org/indicator/SP.DYN.CBRT.IN
DRs <- c(rep(5.73,4),rep(5.12,4)) # death rates from https://data.worldbank.org/indicator/SP.DYN.CBRT.IN

# set model conditions an data frame
timestep = 1/12
traitDF <- data.frame(matrix(ncol = 12, nrow = 0))
colnames(traitDF) <- c("time", "M1", "M2", "M3", "S", "E", "I", "R", "Date", "simulation_number", "Site", "ie")  
traitFileName <- "Concatenated_Data/sensitivity_analyses/SEI-SEIR_simulations_with_immune_heterogeneity_ie.csv"
write.csv(traitDF, traitFileName, row.names = F)

# run simulations ----------------------------------------------------------------------
for (i in 1:length(sites)){
  climateData2 <- subset(climateData, Site == sites[l])
  climateData2 <- climateData2[order(climateData2$Date),]
  climateData2 <- climateData2[complete.cases(climateData2),]
  temp <- climateData2$Temperature
  hum <- climateData2$SVPD
  rain <- climateData2$Two_week_rainfall
  Rmax <- 123
  if (unique(climateData2$country) == "Ecuador"){
    K_trh <- K_trh_quadratic
  } else {
    K_trh <- K_trh_briere
  }
  Date <- climateData2$Date
  H0 <- population[l]
  city <- sites[l]
  BR <- BRs[l]
  DR <- DRs[l]
  times <- seq(1,length(Date), by=1)
  M0 <- K_thr(temp[1], mean(rain), Rmax, H0, timestep)
  parameters <- c(EFD, pEA, MDR, K_thr, a, pMI, mu_th, PDR, b, timestep=timestep)
  for (j in 1:length(ieRates)){
      ie <- ieRates[j]
      out <- ode(y = state, times = times, func = seiseir_model_thr, parms = parameters, method="rk4", atol = 1e-14, rtol = 1e-14, hini = timestep)
      out2 <- as.data.frame(out)
      out2$Date <- Date
      out2$simulation_number <- j
      out2$Site <- sites[i]
      out2$ie <- ie
      traitDF <- rbind(traitDF, out2)
      write.csv(traitDF, traitFileName, row.names = F)
      cat("finished running ode for", sites[i], "simulation #", j, "\n")
  }
}
