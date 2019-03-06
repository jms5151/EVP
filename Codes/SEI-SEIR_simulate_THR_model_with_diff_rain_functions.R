# Temperature, humidity, and rainfall dependent SEI-SEIR model simulations -----------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load packages
library(deSolve)

# load data 
source("Codes/SEI-SEIR_model_THR.R")
source("Codes/SEI-SEIR_simulation_setup.R")
source("Codes/Rainfall_functions.R")

# run simulations
traitDF <- data.frame(matrix(ncol = 13, nrow = 0))
colnames(traitDF) <- c("time", "M1", "M2", "M3", "S", "E", "I", "R", "Date", "Site", "Rain_metiric", "R_function", "Rmax")
traitFileName <- "Concatenated_Data/model_simulations/SEI-SEIR_simulations_THR_diff_rain_functions.csv"
write.csv(traitDF, traitFileName, row.names = F)

rain_types <- c("cumRain", "rainyDays")

for (l in 1:length(sites)){
  siteclimatevars <- names(climateData)[grep(sites[l], names(climateData))]
  climateData2 <- climateData[,c("Date", siteclimatevars)]
  climateData2 <- climateData2[complete.cases(climateData2),]
  temp <- climateData2[, grep('temp', names(climateData2))]
  hum <- climateData2[, grep('humidity', names(climateData2))]
  Date <- climateData2$Date
  H0 <- population[l]
  city <- sites[l]
  BR <- BRs[l]
  DR <- DRs[l]
  times <- seq(1,length(Date), by=1)
  for (m in 1:length(rain_types)){
    rain <- climateData2[, grep(rain_types[m], names(climateData2))]
    if (rain_types[m]=="cumRain"){
      RmaxSeq <- seq(55, 85, 10)
    } else {
      RmaxSeq <- 30
    }
    for (n in 1:length(r_functions)){
      K_thr <- r_functions[[n]]
      for (p in 1:length(RmaxSeq)){
        Rmax <- RmaxSeq[p]
        M0 <- K_thr(temp[1], hum[1], mean(rain), Rmax, H0)
        parameters <- c(EFD, pEA, MDR, K_thr, a, pMI, mu_th, PDR, b, timestep=timestep)
        state <- c(M1 = startIC$m1*M0, M2 = startIC$m2*M0, M3 = startIC$m3*M0, S = startIC$s*H0, E = startIC$e*H0, I = startIC$i*H0, R = startIC$r*H0)
        out <- ode(y = state, times = times, func = seiseir_model_thr, parms = parameters, method="rk4", atol = 1e-14, rtol = 1e-14, hini = timestep)
        out2 <- as.data.frame(out)
        out2$Date <- Date
        out2$Site <- sites[l]
        out2$Rain_metric <- rain_types[m]
        out2$R_function <- r_function_names[n]
        out2$Rmax <- Rmax
        traitDF <- rbind(traitDF, out2)
        write.csv(traitDF, traitFileName, row.names = F)
        cat("finished running ode for", sites[l], rain_types[m], r_function_names[n], Rmax, "\n")
      }
    }  
  }
}

# if testing out rain limiting EFD, pEA, and MDR, use code below
# source("Codes/SEI-SEIR_model_THR.R")
# functionType <- substr(r_function_names[n], 1, 1)
# if (functionType == "K"){
#   K_thr <- r_functions[[n]]
#   seiseir_model <- seiseir_model_thr
#   M0 <- K_thr(temp[1], hum[1], rain[1], Rmax)
# } else if (functionType == "E"){
#   EFD <- r_functions[[n]]
#   seiseir_model <- seiseir_model_thr_efd2
#   M0 <- K_th_efd(temp[1], hum[1], rain[1], Rmax)
# } else if (functionType == "p"){
#   pEA <- r_functions[[n]]
#   seiseir_model <- seiseir_model_thr_pEA2
#   M0 <- K_th_pEA(temp[1], hum[1], rain[1], Rmax)
# } else if (functionType == "M"){
#   MDR <- r_functions[[n]]
#   seiseir_model <- seiseir_model_thr_mdr2
#   M0 <- K_th_mdr(temp[1], hum[1], rain[1], Rmax)
# }
