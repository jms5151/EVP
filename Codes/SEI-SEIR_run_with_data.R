rm(list=ls()) #remove previous variable assignments
source("Codes/SEI-SEIR_model_script_in_R.R")
library(deSolve)

#------------------- load temperature data files (need for temp dependent and independ. models)
climateData <- read.csv("Concatenated_Data/climate_data_kenya.csv", head=T)

# ----------------- set up data for model
# meanTemp <- list(climateData$GF_Chulaimbo_mean_temp, climateData$GF_Kisumu_mean_temp)
# dates <- list(west.clim$Date, west.clim$Date, coast.clim$Date, coast.clim$Date)
population <- c(5336, 419072,14444,75357)
sites <- c("Chulaimbo", "Kisumu", "Msambweni", "Ukunda")
timestep = 1/12
# seedPattern <- c(rep(0,90), .01*population[j])
# seed <- rep(seedPattern, length(Date))

# ----------------- Run SEI-SEIR model with temperature data
# kisumu: M1 = .9994*M0, M2 = 0.0003*M0, M3 = 0.0003*M0, S = 0.5*H0, E = 0.00003*H0, I = 0, R = 0.49997*H0

# for (j in 1:length(sites)){
  # t_colName <- paste0("GF_", sites[j], "_mean_temp")
  # temp <- climateData[,t_colName]
  # M0 <- K_t(temp[1]) # maybe determine this based on 25 degrees rather than actual starting temp
  # Date <- as.Date(climateData$Date, "%Y-%m-%d")
  # H0 <- population[j]
  # city <- sites[j]
  # state <- c(M1 = .9994*M0, M2 = 0.0003*M0, M3 = 0.0003*M0, S = 0.5*H0, E = 0.00003*H0, I = 0, R = 0.49997*H0) # initial conditions, R = hcc study for denv 50/3427 children were inititally infected; previously S = 0.9975*H0 and R = 0
  # times <- seq(1,length(Date), by=1) # times from temperature data, needs to be in days
  # parameters <- c(EFD, pEA, MDR, K_t, a, pMI, mu_t, PDR, b, timestep=timestep)
  # out <- ode(y = state, times = times, func = seiseir_model_t, parms = parameters, method="rk4", atol = 1e-14, rtol = 1e-14, hini = timestep)
  # out2 <- as.data.frame(out)
  # out2$totalMosquitoPop <- out2$M1 + out2$M2 + out2$M3
  # out2$Date <- Date[1:nrow(out)]
  # write.csv(out2, paste0("Kenya/Concatenated_Data/SEI-SEIR/", "SEI-SEIR_Simulations_TempOnly_", city, ".csv"), row.names=F)
  # filename <-  paste0("Kenya/Figures/SEI-SEIR-Output/", "TempOnly_", city, ".tiff")
  # tiff(file=filename,  height = 544, width = 900)
  # plot(out, type = "l", lwd = 2)
  # plot(times, temp, type="l", main=c("Mean Temperature"))
  # dev.off()
# }


# Run SEI-SEIR model with temperature and humidity data  -----------------
# for (j in 1:length(sites)){
  # t_colName <- paste0("GF_", sites[j], "_mean_temp")
  # temp <- climateData[,t_colName]
  # h_colName <- paste0("GF_", sites[j], "_humidity")
  # hum <- climateData[,h_colName]
  # M0 <- K_th(temp[1], hum[1])
  # Date <- as.Date(climateData$Date, "%Y-%m-%d")
  # H0 <- population[j]
  # city <- sites[j]
  # state <- c(M1 = .9994*M0, M2 = 0.0003*M0, M3 = 0.0003*M0, S = 0.5*H0, E = 0.00003*H0, I = 0, R = 0.49997*H0) # initial conditions, R = hcc study for denv 50/3427 children were inititally infected; previously S = 0.9975*H0 and R = 0
  # times <- seq(1,length(Date), by=1) # times from temperature data, needs to be in days
  # parameters <- c(EFD, pEA, MDR, K_th, a, pMI, mu_th, PDR, b, timestep=timestep)
  # out <- ode(y = state, times = times, func = seiseir_model_th, parms = parameters, method="rk4", atol = 1e-14, rtol = 1e-14, hini = timestep)
  # out2 <- as.data.frame(out)
  # out2$totalMosquitoPop <- out2$M1 + out2$M2 + out2$M3
  # out2$Date <- Date[1:nrow(out)]
  # write.csv(out2, paste0("Kenya/Concatenated_Data/SEI-SEIR/", "SEI-SEIR_Simulations_TempOnly_", city, ".csv"), row.names=F)
  # filename <-  paste0("Kenya/Figures/SEI-SEIR-Output/", "TempOnly_", city, ".tiff")
  # tiff(file=filename,  height = 544, width = 900)
  # plot(out, type = "l", lwd = 2)
  # plot(times, temp, type="l", main=c("Mean Temperature"))
  # dev.off()
# }


# Run SEI-SEIR model with temperature, humidity and rainfall data ----------------------------
for (j in 1:length(sites)){
  t_colName <- paste0("GF_", sites[j], "_mean_temp")
  temp <- climateData[,t_colName]
  h_colName <- paste0("GF_", sites[j], "_humidity")
  hum <- climateData[,h_colName]
  r_colName <- paste0("GF_", sites[j], "_cumRain")
  rain <- climateData[,r_colName]
  M0 <- K_thr(temp[1], hum[1], rain[1])
  Date <- as.Date(climateData$Date, "%Y-%m-%d")
  H0 <- population[j]
  city <- sites[j]
  state <- c(M1 = .9994*M0, M2 = 0.0003*M0, M3 = 0.0003*M0, S = 0.5*H0, E = 0.00003*H0, I = 0, R = 0.49997*H0) # initial conditions, R = hcc study for denv 50/3427 children were inititally infected; previously S = 0.9975*H0 and R = 0
  times <- seq(1,length(Date), by=1) # times from temperature data, needs to be in days
  parameters <- c(EFD, pEA, MDR, K_thr, a, pMI, mu_th, PDR, b, timestep=timestep)
  out <- ode(y = state, times = times, func = seiseir_model_thr, parms = parameters, method="rk4", atol = 1e-14, rtol = 1e-14, hini = timestep)
  # out <- dede(y = state, times = times, func = seiseir_model_thr, parms = parameters, method='vode')
  # out2 <- as.data.frame(out)
  plot(out, type = "l", lwd = 2)
}

#----------------------------------------------------------------------------------------------------------
# ------ Run SEI-SEIR model as temperature independent model (mean temperature=single rate for each train)
#----------------------------------------------------------------------------------------------------------

# timestep = 1/12
# 
# x <- c(Daily_T_Chulaimbo$temp_mean_hobo, Daily_T_Kisumu$temp_mean_hobo, Daily_T_Msambweni$temp_mean_hobo, Daily_T_Ukunda$temp_mean_hobo)
# meanT <- 29#mean(x)
# 
# for (j in 1:4){
#   temp <- rep(meanT, length(meanTemp[[j]]))
#   M0 <- K(temp[1])
#   Date <- dates[[j]]
#   numDates <- length(Date)
#   if (j == 1) {
#     H0 <- 5336#2940 #5336 * 0.5510 = Maseno 2015 population projection * proportion of children in Kisumu county
#     city <- "Chulaimbo"
#   } else if (j == 2){
#     H0 <-  419072 #230909 #419072 * 0.5510 = Kisumu 2015 population projection * proportion of children in Kisumu county
#     city <- "Kisumu"
#   } else if (j == 3){
#     H0 <- 14444 #8317 #14444 * 0.5758 = Msambweni 2015 population projection * proportion of children in Kwale county
#     city <- "Msambweni"
#   } else if (j == 4){
#     H0 <- 75357 #43391 #75357 * 0.5758 = Ukunda 2015 population projection * proportion of children in Kwale county
#     city <- "Ukunda"
#   }
#   # state <- c(M1 = 0.985*M0, M2 = 0, M3 = 0.015*M0, S = 0.98291*H0, E = 0, I = 0.0025*H0, R = 0.01459*H0) # initial conditions, R = hcc study for denv 50/3427 children were inititally infected; previously S = 0.9975*H0 and R = 0
#   state <- c(M1 = 0.985*M0, M2 = 0, M3 = 0.015*M0, S = 0.9975*H0, E = 0, I = 0.0025*H0, R = 0) # initial conditions, R = hcc study for denv 50/3427 children were inititally infected; previously S = 0.9975*H0 and R = 0
#   times <- seq(1,numDates, by=1) # times from temperature data, needs to be in days
#   parameters <- c(EFD, pEA, MDR, K, a, pMI, mu, PDR, b, timestep=timestep)
#   out <- ode(y = state, times = times, func = seiseir_model, parms = parameters,  method="rk4", atol = 1e-14, rtol = 1e-14, hini = timestep)
#   out2 <- as.data.frame(out)
#   out2$totalMosquitoPop <- out2$M1 + out2$M2 + out2$M3
#   out2$Date <- Date[1:nrow(out)]
#   write.csv(out2, paste0("C:/Users/Jamie/Box Sync/DENV/Concatenated_Data/SEI-SEIR/Null_Model_25.9C/", "SEI-SEIR_Simulations_29C_", city, ".csv"), row.names=F)
#   filename <-  paste0("C:/Users/Jamie/Box Sync/DENV/Figures/SEI-SEIR-Output/", "Null_29C_", city, ".tiff")
#   tiff(file=filename,  height = 544, width = 900)
#   plot(out, type = "l", lwd = 2)
#   plot(times, temp, type="l", main=c("Mean Temperature"))
#   dev.off()
# }
# 
# # ----------------- Run SEI-SEIR model with real temperature and humidity data
# meanRH <- lapply(df, function(i) i$Humidity_mean)
# 
# for (j in 1:4){
#   temp <- meanTemp[[j]]
#   M0 <- K(temp[1])
#   Date <- dates[[j]]
#   numDates <- length(Date)
#   rh <- meanRH[[j]]
#   if (j == 1) {
#     H0 <- 554662
#     city <- "Chulaimbo"
#   } else if (j == 2){
#     H0 <- 968879
#     city <- "Kisumu"
#   } else if (j == 3){
#     H0 <- 288393
#     city <- "Msambweni"
#   } else if (j == 4){
#     H0 <- 867663
#     city <- "Ukunda"
#   }
#   state <- c(M1 = 0.985*M0, M2 = 0, M3 = 0.015*M0, S = 0.9975*H0, E = 0, I = 0.0025*H0, R = 0) # initial conditions
#   times <- seq(1,numDates, by=1) # times from temperature data, needs to be in days
#   parameters <- c(EFD, pEA_th, MDR, K, a, pMI, mu_th, PDR, b)
#   out <- ode(y = state, times = times, func = seiseir_model_th, parms = parameters,  method="rk4", atol = 1e-14, rtol = 1e-14)
#   out2 <- as.data.frame(out)
#   out2$totalMosquitoPop <- out2$M1 + out2$M2 + out2$M3
#   out2$Date <- Date[1:nrow(out)]
#   write.csv(out2, paste0("C:/Users/Jamie/Box Sync/DENV/Concatenated_Data/SEI-SEIR/", "SEI-SEIR_Simulations_", city, "_TempHumidity", ".csv"), row.names=F)
#   filename <-  paste0("C:/Users/Jamie/Box Sync/DENV/Figures/SEI-SEIR-Output/", "TempHumidity_",  city, ".tiff")
#   tiff(file=filename,  height = 544, width = 900)
#   plot(out, type = "l", lwd = 2)
#   plot(times, temp, type="l", main=c("Mean Temperature"))
#   plot(times, rh, type="l", main=c("Mean Humidity"))
#   dev.off()
# }
# 
# 
# ## compare times with different ode solvers; rk4 is fastest
# print(system.time(out <- ode(y = state, times = times, func = seiseir_model, parms = parameters,  method="rk4", atol = 1e-14, rtol = 1e-14)))
# print(system.time(out <- ode(y = state, times = times, func = seiseir_model, parms = parameters,  method="lsode", atol = 1e-14, rtol = 1e-14)))
# print(system.time(out <- ode(y = state, times = times, func = seiseir_model, parms = parameters,  method="lsoda", atol = 1e-14, rtol = 1e-14)))
# print(system.time(out <- ode(y = state, times = times, func = seiseir_model, parms = parameters,  method="lsodes", atol = 1e-14, rtol = 1e-14)))
# print(system.time(out <- ode(y = state, times = times, func = seiseir_model, parms = parameters,  method="vode", atol = 1e-14, rtol = 1e-14)))
# print(system.time(out <- ode(y = state, times = times, func = seiseir_model, parms = parameters,  method="ode45", atol = 1e-14, rtol = 1e-14)))
# 
