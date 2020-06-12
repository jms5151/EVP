# Rainfall dependent SEI-SEIR model simulations -----------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load packages
library(deSolve)

# load model
source("Codes/SEI-SEIR_model_R.R")

# load data 
source("Codes/SEI-SEIR_simulation_setup.R")

# run simulations
traitDF <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(traitDF) <- c("time", "M1", "M2", "M3", "S", "E", "I", "R", "Date", "Site")
traitFileName <- "Concatenated_Data/model_simulations/SEI-SEIR_simulations_rainfall.csv"
write.csv(traitDF, traitFileName, row.names = F)

for (l in 1:length(sites)){
  siteclimatevars <- names(climateData)[grep(sites[l], names(climateData))]
  climateData2 <- climateData[,c("Date", siteclimatevars)]
  climateData2 <- climateData2[complete.cases(climateData2),]
  temp <- mean(climateData2[, grep('temp', names(climateData2))])
  rain <- climateData2[, grep('cumRain', names(climateData2))]
  H0 <- population[l]
  M0 <- K_tr(temp, rain[1], H0)
  Date <- climateData2$Date
  city <- sites[l]
  BR <- BRs[l]
  DR <- DRs[l]
  times <- seq(1,length(Date), by=1)
  parameters <- c(EFD, pEA, MDR, K_tr, a, pMI, mu_t, PDR, b, timestep=timestep)
  state <- c(M1 = startIC$m1*M0, M2 = startIC$m2*M0, M3 = startIC$m3*M0, S = startIC$s*H0, E = startIC$e*H0, I = startIC$i*H0, R = startIC$r*H0)
  out <- ode(y = state, times = times, func = seiseir_model_r, parms = parameters, method="rk4", atol = 1e-14, rtol = 1e-14, hini = timestep)
  out2 <- as.data.frame(out)
  out2$Date <- Date
  out2$Site <- sites[l]
  traitDF <- rbind(traitDF, out2)
  write.csv(traitDF, traitFileName, row.names = F)
  cat("finished running ode for", sites[l], "\n")
}
