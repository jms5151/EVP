# Test initial conditions for SEI-SEIR model ----------------------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load packages
library(dplyr)
library(deSolve)

# load model 
source("Codes/SEI-SEIR_model_THR.R")

# load data 
climateData <- read.csv("Concatenated_Data/climate_data/merged_climate_data.csv", head=T)
climateData$Date <- as.Date(climateData$Date, "%Y-%m-%d")

initCond <- read.csv("Concatenated_Data/sensitivity_analyses/LHS_inputs.csv", head=T, stringsAsFactors = F)

# run simulations using different initial conditions for state variables and save results -------------
population <- c(7304, 547557, 240698, 154048, 57370, 279890, 13670, 25620)
sites <- c("Chulaimbo", "Kisumu", "Msambweni", "Ukunda", "Huaquillas", "Machala", "Portovelo", "Zaruma")
BRs <- c(rep(31.31,4),rep(20.18,4)) # birth rates from https://data.worldbank.org/indicator/SP.DYN.CBRT.IN
DRs <- c(rep(5.73,4),rep(5.12,4)) # death rates from https://data.worldbank.org/indicator/SP.DYN.CBRT.IN
ie <- 0.01 # immigration/emigration rate
timestep = 1/12
initDF <- data.frame(matrix(ncol = 11, nrow = 0))
colnames(initDF) <- c("time", "M1", "M2", "M3", "S", "E", "I", "R", "Date", "Site", "InitCond")  
fileName <- "Concatenated_Data/sensitivity_analyses/Initital_condition_simulations.csv"
write.csv(initDF, fileName, row.names=F)

# run model simulations
for (j in 1:length(sites)){
  for (k in 1:nrow(initCond)){
    siteclimatevars <- names(climateData)[grep(sites[i], names(climateData))]
    climateData2 <- climateData[,c("Date", siteclimatevars)]
    climateData2 <- climateData2[complete.cases(climateData2),]
    temp <- climateData2[, grep('temp', names(climateData2))]
    hum <- climateData2[, grep('humidity', names(climateData2))]
    rain <- climateData2[, grep('cumRain', names(climateData2))]
    M0 <- K_thr(temp[1], hum[1], rain[1]) # this changes depending on model!
    Date <- climateData2$Date
    H0 <- population[j]
    city <- sites[j]
    times <- seq(1,length(Date), by=1)
    parameters <- c(EFD, pEA, MDR, K_thr, a, pMI, mu_th, PDR, b, timestep=timestep) # this changes too!
    state <- c(M1 = initCond$m1[k]*M0, M2 = initCond$m2[k]*M0, M3 = initCond$m3[k]*M0, S = initCond$s[k]*H0, E = initCond$e[k]*H0, I = initCond$i[k]*H0, R = initCond$r[k]*H0)
    out <- ode(y = state, times = times, func = seiseir_model_r, parms = parameters, method="rk4", atol = 1e-14, rtol = 1e-14, hini = timestep)
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
inits <- read.csv("Concatenated_Data/sensitivity_analyses/LHS_inputs.csv", head=T)
startIC <- subset(inits, IC == "18")

# run simulations using different start dates and save results -------------------------------
# this will differ between kenya and ecuador, not sure if it is worth doing with ecuador
startDate <- seq.Date(min(climateData$Date),(min(climateData$Date)+168),by="weeks")
initSDDF <- data.frame(matrix(ncol = 11, nrow = 0))
colnames(initSDDF) <- c("time", "M1", "M2", "M3", "S", "E", "I", "R", "Date", "Site", "StartDate")  
fileName2 <- "Concatenated_Data/sensitivity_analyses/Initital_condition_simulations_diff_start_dates.csv"
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
    M0 <- K_thr(temp[1], hum[1], rain[1]) # this will change with model!
    Date <- climateData2$Date
    H0 <- population[j]
    city <- sites[j]
    times <- seq(1,length(Date), by=1)
    parameters <- c(EFD, pEA, MDR, K_thr, a, pMI, mu_th, PDR, b, timestep=timestep) # this will change with model
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
