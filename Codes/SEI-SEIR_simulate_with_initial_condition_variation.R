# Test initial conditions for SEI-SEIR model ----------------------------------------
rm(list=ls()) #remove previous variable assignments

# load model ------------------------------------------------------------------------
source("Codes/SEI-SEIR_model_script_in_R.R")

# load climate data -----------------------------------------------------------------
climateData <- read.csv("Kenya/Concatenated_Data/climate/gapfilled_climate_data.csv", head=T)
climateData$Date <- as.Date(climateData$Date, "%Y-%m-%d")

# load packages ---------------------------------------------------------------------
library(dplyr)
library(lhs)
library(deSolve)

# create latin hypercube to select initial conditions for state variables -----------
lhs_design <- optimumLHS(n = 50, k = 7, verbose = FALSE)
lhs_design <- as.data.frame(lhs_design)
colnames(lhs_design) <- c("m1", "m2", "m3", "s", "e", "i", "r")

# divide to make SEI = 1 and SEIR = 1
lhs_design$IC <- seq(1:nrow(lhs_design))
lhs_design$total.m <- rowSums(lhs_design[1:3])
lhs_design$total.h <- rowSums(lhs_design[4:7])
lhs_design$m1 <- lhs_design$m1/lhs_design$total.m
lhs_design$m2 <- lhs_design$m2/lhs_design$total.m
lhs_design$m3 <- lhs_design$m3/lhs_design$total.m
lhs_design$s <- lhs_design$s/lhs_design$total.h
lhs_design$e <- lhs_design$e/lhs_design$total.h
lhs_design$i <- lhs_design$i/lhs_design$total.h
lhs_design$r <- lhs_design$r/lhs_design$total.h

initCond <- lhs_design[,1:8]

# save sampling design
write.csv(initCond, "Kenya/Concatenated_Data/SEI-SEIR/Initital_conditions/LHS_inputs.csv", row.names = F)

# run simulations using different initial conditions for state variables and save results -------------
population <- c(5336, 419072,14444,75357)
sites <- c("Chulaimbo", "Kisumu", "Msambweni", "Ukunda")
timestep = 1/12
initDF <- data.frame(matrix(ncol = 11, nrow = 0))
colnames(initDF) <- c("time", "M1", "M2", "M3", "S", "E", "I", "R", "Date", "Site", "InitCond")  
fileName <- "Kenya/Concatenated_Data/SEI-SEIR/Initital_conditions/Initital_condition_simulations.csv"
write.csv(initDF, fileName, row.names=F)

# run model simulations
for (j in 1:length(sites)){
  for (k in 1:nrow(initCond)){
    t_colName <- paste0("GF_", sites[j], "_mean_temp")
    temp <- climateData[,t_colName]
    h_colName <- paste0("GF_", sites[j], "_humidity")
    hum <- climateData[,h_colName]
    r_colName <- paste0("GF_", sites[j], "_cumRain")
    rain <- climateData[,r_colName]
    M0 <- K_thr(temp[1], hum[1], rain[1])
    Date <- climateData$Date
    H0 <- population[j]
    city <- sites[j]
    times <- seq(1,length(Date), by=1)
    parameters <- c(EFD, pEA, MDR, K_thr, a, pMI, mu_th, PDR, b, timestep=timestep)
    state <- c(M1 = initCond$m1[k]*M0, M2 = initCond$m2[k]*M0, M3 = initCond$m3[k]*M0, S = initCond$s[k]*H0, E = initCond$e[k]*H0, I = initCond$i[k]*H0, R = initCond$r[k]*H0)
    out <- ode(y = state, times = times, func = seiseir_model_thr, parms = parameters, method="rk4", atol = 1e-14, rtol = 1e-14, hini = timestep)
    out2 <- as.data.frame(out)
    out2$Date <- Date
    out2$Site <- sites[j]
    out2$IC <- c(initCond$IC[k])
    initDF <- rbind(initDF, out2)
    write.csv(initDF, fileName, row.names = F)
    cat("finished running ode for", sites[j], "Initial conditions =", initCond$IC[k], "\n")
  }
}

# select initial conditions for future simulations -------------------------------------------
inits <- read.csv("Kenya/Concatenated_Data/SEI-SEIR/Initital_conditions/LHS_inputs.csv", head=T)
startIC <- subset(inits, IC == "18")

# run simulations using different start dates and save results -------------------------------
startDate <- seq.Date(min(climateData$Date),(min(climateData$Date)+168),by="weeks")
initSDDF <- data.frame(matrix(ncol = 11, nrow = 0))
colnames(initSDDF) <- c("time", "M1", "M2", "M3", "S", "E", "I", "R", "Date", "Site", "StartDate")  
fileName2 <- "Kenya/Concatenated_Data/SEI-SEIR/Initital_conditions/Initital_condition_simulations_diff_start_dates.csv"
write.csv(initSDDF, fileName2, row.names=F)

# run model simulations
for (j in 1:length(sites)){
  for (k in 1:length(startDate)){
    climateData2 <- subset(climateData, Date >= startDate[k])
    t_colName <- paste0("GF_", sites[j], "_mean_temp")
    temp <- climateData2[,t_colName]
    h_colName <- paste0("GF_", sites[j], "_humidity")
    hum <- climateData2[,h_colName]
    r_colName <- paste0("GF_", sites[j], "_cumRain")
    rain <- climateData2[,r_colName]
    M0 <- K_thr(temp[1], hum[1], rain[1])
    Date <- climateData2$Date
    H0 <- population[j]
    city <- sites[j]
    times <- seq(1,length(Date), by=1)
    parameters <- c(EFD, pEA, MDR, K_thr, a, pMI, mu_th, PDR, b, timestep=timestep)
    state <- c(M1 = startIC$m1*M0, M2 = startIC$m2*M0, M3 = startIC$m3*M0, S = startIC$s*H0, E = startIC$e*H0, I = startIC$i*H0, R = startIC$r*H0)
    out <- ode(y = state, times = times, func = seiseir_model_thr, parms = parameters, method="rk4", atol = 1e-14, rtol = 1e-14, hini = timestep)
    out2 <- as.data.frame(out)
    out2$Date <- Date
    out2$Site <- sites[j]
    out2$StartDate <- c(startDate[k])
    initSDDF <- rbind(initSDDF, out2)
    write.csv(initSDDF, fileName2, row.names = F)
    cat("finished running ode for", sites[j], "Initial conditions =", as.character(startDate[k]), "\n")
  }
}

# combine LHS initial conditions for state variables with varying start dates
# initCond <- data.frame(lhs_design[1:8]) %>% merge(startDate, by=NULL) 
# colnames(initCond)[ncol(initCond)] <- "startDate"
