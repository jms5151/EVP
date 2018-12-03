# Temperature dependent SEI-SEIR model simulations -----------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load packages
library(deSolve)

# load model
source("Codes/SEI-SEIR_model_H.R")

# load climate data
climateData <- read.csv("Concatenated_Data/climate_data/merged_climate_data.csv", head=T, stringsAsFactors = F)
climateData$Date <- as.Date(climateData$Date, "%Y-%m-%d")

# load and set initial conditions
init.cond <- read.csv("Concatenated_Data/sensitivity_analyses/LHS_inputs.csv", head=T)
startIC <- subset(init.cond, IC == "18")

# set immigration and emmigration rate
ie <- 0.01

# run simulations
population <- c(5336, 419072,14444,75357, 57370, 279890, 13670, 25620)
sites <- c("Chulaimbo", "Kisumu", "Msambweni", "Ukunda", "Huaquillas", "Machala", "Portovelo", "Zaruma")
timestep = 1/12
traitDF <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(traitDF) <- c("time", "M1", "M2", "M3", "S", "E", "I", "R", "Date", "Site")
traitFileName <- "Concatenated_Data/model_simulations/SEI-SEIR_simulations_humidity.csv"
write.csv(traitDF, traitFileName, row.names = F)

for (l in 1:length(sites)){
  siteclimatevars <- names(climateData)[grep(sites[l], names(climateData))]
  climateData2 <- climateData[,c("Date", siteclimatevars)]
  climateData2 <- climateData2[complete.cases(climateData2),]
  temp <- mean(climateData2[, grep('temp', names(climateData2))])
  hum <- climateData2[, grep('humidity', names(climateData2))]
  M0 <- K_th(temp, hum[1])
  Date <- climateData2$Date
  H0 <- population[l]
  city <- sites[l]
  times <- seq(1,length(Date), by=1)
  parameters <- c(EFD, pEA, MDR, K_th, a, pMI, mu_th, PDR, b, timestep=timestep)
  state <- c(M1 = startIC$m1*M0, M2 = startIC$m2*M0, M3 = startIC$m3*M0, S = startIC$s*H0, E = startIC$e*H0, I = startIC$i*H0, R = startIC$r*H0)
  out <- ode(y = state, times = times, func = seiseir_model_h, parms = parameters, method="rk4", atol = 1e-14, rtol = 1e-14, hini = timestep)
  out2 <- as.data.frame(out)
  out2$Date <- Date
  out2$Site <- sites[l]
  traitDF <- rbind(traitDF, out2)
  write.csv(traitDF, traitFileName, row.names = F)
  cat("finished running ode for", sites[l], "\n")
}

