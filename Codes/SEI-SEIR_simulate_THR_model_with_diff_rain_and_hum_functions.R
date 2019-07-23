# Temperature, humidity, and rainfall dependent SEI-SEIR model simulations -----------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load packages
library(deSolve)

# load data 
source("Codes/SEI-SEIR_model_THR.R")
source("Codes/SEI-SEIR_simulation_setup.R")
source("Codes/Rainfall_and_humidity_joint_functions.R")

# run simulations
traitDF <- data.frame(matrix(ncol = 13, nrow = 0))
# colnames(traitDF) <- c("time", "M1", "M2", "M3", "S", "E", "I", "R", "Date", "Site", "Rain_metiric", "Humidity_function", "Rain_function")
traitFileName <- "Concatenated_Data/model_simulations/SEI-SEIR_simulations_THR_diff_rain_and_hum_functions.csv"
write.csv(traitDF, traitFileName, row.names = F)

rh_functions <- list(K_thr_quadratic, K_thr_right_skewed, K_thr_briere)
rh_names <- c("Rainfall_quadratic", "Rainfall_right_skewed", "Rainfall_briere")

rain_types <- c("Monthly_rainfall", "Monthly_rainfall_weighted")#, "cumRain", "rainyDays")
humidity_types <- c("Humidity_mu_linear-a_linear", "Humidity_mu_linear-a_sigmoidal", "Humidity_mu_sigmoidal-a_linear", "Humidity_mu_sigmoidal-a_sigmoidal")

for (l in 1:length(sites)){
  climateData2 <- subset(climateData, Site == sites[l])
  climateData2 <- climateData2[order(climateData2$Date),]
  climateData2 <- climateData2[complete.cases(climateData2),]
  temp <- climateData2$Temperature
  hum <- climateData2$Humidity
  Date <- climateData2$Date
  H0 <- population[l]
  city <- sites[l]
  BR <- BRs[l]
  DR <- DRs[l]
  times <- seq(1,length(Date), by=1)
  for (m in 1:length(rain_types)){
    rain <- climateData2[, rain_types[m]]
    if (rain_types[m]=="Monthly_rainfall"){
      Rmax <- 400
    } else if (rain_types[m]=="Monthly_rainfall_weighted") {
      Rmax <- 55
    }
    for (HUM in 1:length(humidity_types)){
      if (humidity_types[HUM] == "Humidity_mu_linear-a_linear"){
        mu_th <- mu_th_linear
        a <- a_linear
      } else if (humidity_types[HUM] == "Humidity_mu_linear-a_sigmoidal") {
        mu_th <- mu_th_linear
        a <- a_sigmoidal
      } else if (humidity_types[HUM] == "Humidity_mu_sigmoidal-a_linear") {
        mu_th <- mu_th_sigmoidal
        a <- a_linear
      } else if (humidity_types[HUM] == "Humidity_mu_sigmoidal-a_sigmoidal") {
        mu_th <- mu_th_sigmoidal
        a <- a_sigmoidal
      }
      for (n in 1:length(rh_functions)){
        K_thr <- rh_functions[[n]]
        M0 <- K_thr(temp[1], hum[1], mean(rain), Rmax, H0)
        parameters <- c(EFD, pEA, MDR, K_thr, a, pMI, mu_th, PDR, b, timestep=timestep)
        state <- c(M1 = startIC$m1*M0, M2 = startIC$m2*M0, M3 = startIC$m3*M0, S = startIC$s*H0, E = startIC$e*H0, I = startIC$i*H0, R = startIC$r*H0)
        out <- ode(y = state, times = times, func = seiseir_model_thr, parms = parameters, method="rk4", atol = 1e-14, rtol = 1e-14, hini = timestep)
        out2 <- as.data.frame(out)
        out2$Date <- Date
        out2$Site <- sites[l]
        out2$Rain_metric <- rain_types[m]
        out2$Humidity_function <- humidity_types[HUM]
        out2$Rain_function <- rh_names[n]
        traitDF <- rbind(traitDF, out2)
        write.csv(traitDF, traitFileName, row.names = F)
        cat("finished running ode for", sites[l], rain_types[m], humidity_types[HUM], rh_names[n], "\n")
      }
    }  
  }
}
