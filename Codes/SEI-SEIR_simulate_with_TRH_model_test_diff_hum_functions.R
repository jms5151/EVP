# Temperature, humidity, and rainfall dependent SEI-SEIR model simulations -----------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load packages
library(deSolve)

# load data 
source("Codes/SEI-SEIR_model_TRH.R")
source("Codes/SEI-SEIR_simulation_setup.R")

# run simulations
traitDF <- data.frame(matrix(ncol = 11, nrow = 0))
colnames(traitDF) <- c("time", "M1", "M2", "M3", "S", "E", "I", "R", "Date", "Site", "Humidity_function")
traitFileName <- "Concatenated_Data/model_simulations/SEI-SEIR_simulations_TRH_test_diff_hum_functions.csv"
write.csv(traitDF, traitFileName, row.names = F)

rh_functions <- list(mu_th_linear, mu_th_sigmoidal) 
rh_names <- c("linear", "sigmoidal")

for (l in 1:length(sites)){
  climateData2 <- subset(climateData, Site == sites[l])
  climateData2 <- climateData2[order(climateData2$Date),]
  climateData2 <- climateData2[complete.cases(climateData2),]
  temp <- climateData2$Temperature
  rain <- climateData2$Monthly_rainfall
  Rmax <- mean(rain) + 2*sd(rain)
  if (unique(climateData2$country) == "Ecuador"){
    K_trh <- K_trh_quadratic
  } else {
    K_trh <- K_trh_briere
  }
  hum <- climateData2$Humidity
  Date <- climateData2$Date
  H0 <- population[l]
  city <- sites[l]
  BR <- BRs[l]
  DR <- DRs[l]
  times <- seq(1,length(Date), by=1)
  for (HUM in 1:length(rh_names)){
    mu_th <- rh_functions[[HUM]]
    M0 <- K_trh(temp[1], hum[1], mean(rain), Rmax, H0)
    parameters <- c(EFD, pEA, MDR, K_trh, a, pMI, mu_th, PDR, b, timestep=timestep)
    state <- c(M1 = startIC$m1*M0, M2 = startIC$m2*M0, M3 = startIC$m3*M0, S = startIC$s*H0, E = startIC$e*H0, I = startIC$i*H0, R = startIC$r*H0)
    out <- ode(y = state, times = times, func = seiseir_model_thr, parms = parameters, method="rk4", atol = 1e-14, rtol = 1e-14, hini = timestep)
    out2 <- as.data.frame(out)
    out2$Date <- Date
    out2$Site <- sites[l]
    out2$Humidity_function <- rh_names[HUM]
    traitDF <- rbind(traitDF, out2)
    write.csv(traitDF, traitFileName, row.names = F)
    cat("finished running ode for", sites[l], rh_names[HUM], "\n")
  }
}
