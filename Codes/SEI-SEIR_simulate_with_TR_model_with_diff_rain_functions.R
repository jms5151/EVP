# Temperature, humidity, and rainfall dependent SEI-SEIR model simulations -----------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load packages
library(deSolve)

# load data 
source("Codes/SEI-SEIR_model_TR.R")
source("Codes/SEI-SEIR_simulation_setup.R")

# run simulations
traitDF <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(traitDF) <- c("time", "M1", "M2", "M3", "S", "E", "I", "R", "Date", "Site")
traitFileName <- "Concatenated_Data/model_simulations/SEI-SEIR_simulations_TR_diff_rain_functions_weighted.csv"
write.csv(traitDF, traitFileName, row.names = F)

for (i in 1:length(sites)){
  climateData2 <- subset(climateData, Site == sites[i])
  temp <- climateData2$Temperature
  rain <- climateData2$Monthly_rainfall_weighted
  Date <- climateData2$Date
  H0 <- population[i]
  city <- sites[i]
  BR <- BRs[i]
  DR <- DRs[i]
  times <- seq(1,length(Date), by=1)
  # Rmax = 400
  Rmax = 55
  if (unique(climateData2$country) == "Ecuador"){
    k_tr <- K_tr_right_skewed
  } else {
    k_tr <- K_tr_briere
  }
  M0 <- K_tr(temp[1], rain[1], Rmax, H0)
  parameters <- c(EFD, pEA, MDR, K_tr, a, pMI, mu_t, PDR, b, timestep=timestep)
  state <- c(M1 = startIC$m1*M0, M2 = startIC$m2*M0, M3 = startIC$m3*M0, S = startIC$s*H0, E = startIC$e*H0, I = startIC$i*H0, R = startIC$r*H0)
  out <- ode(y = state, times = times, func = seiseir_model_tr, parms = parameters, method="rk4", atol = 1e-14, rtol = 1e-14, hini = timestep)
  out2 <- as.data.frame(out)
  out2$Date <- Date
  out2$Site <- sites[i]
  traitDF <- rbind(traitDF, out2)
  write.csv(traitDF, traitFileName, row.names = F)
  cat("finished running ode for", sites[i], "\n")
}
