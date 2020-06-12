# Temperature, humidity, and rainfall dependent SEI-SEIR model simulations -----------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load packages
library(deSolve)

# load data 
source("SEI-SEIR_model.R")
source("SEI-SEIR_simulation_setup.R")

# run simulations
traitDF <- data.frame(matrix(ncol = 11, nrow = 0))
colnames(traitDF) <- c("time", "M1", "M2", "M3", "S", "E", "I", "R", "Date", "Site", "Rain_function")
traitFileName <- "SEI-SEIR_simulations_THR_test_different_rain_functions.csv"
write.csv(traitDF, traitFileName, row.names = F)

rfunctions_names <- c("Briere", "Quadratic", "Inverse")
rfunctions <- list(K_thr_briere, K_thr_quadratic, K_thr_inverse)

for (l in 1:length(sites)){
  climateData2 <- subset(climateData, Site == sites[l])
  climateData2 <- climateData2[order(climateData2$Date),]
  climateData2 <- climateData2[complete.cases(climateData2),]
  temp <- climateData2$Temperature
  rain <- climateData2$Two_week_rainfall
  Rmax <- 123
  hum <- climateData2$SVPD
  Date <- climateData2$Date
  N <- population[l]
  city <- sites[l]
  BR <- BRs[l]
  DR <- DRs[l]
  times <- seq(1,length(Date), by=1)
  for (k in 1:length(rfunctions_names)){
    K_thr <- rfunctions[[k]]
    M0 <- K_thr(temp[1], hum[1], rain[1], Rmax, N)
    parameters <- c(EFD, pEA, MDR, K_thr, a, pMI, mu_th, PDR, b, timestep=timestep)
    state <- c(M1 = startIC$m1*M0, M2 = startIC$m2*M0, M3 = startIC$m3*M0, S = startIC$s*N, E = startIC$e*N, I = startIC$i*N, R = startIC$r*N)
    out <- ode(y = state, times = times, func = seiseir_model_thr, parms = parameters, method="rk4", atol = 1e-14, rtol = 1e-14, hini = timestep)
    out2 <- as.data.frame(out)
    out2$Date <- Date
    out2$Site <- sites[l]
    out2$Rain_function <- rfunctions_names[k]
    write.table(out2, file=traitFileName, row.names = F, sep = ",", col.names = !file.exists(traitFileName), append = T)
    cat("finished running ode for", sites[l], rfunctions_names[k], "\n")
  }
}
