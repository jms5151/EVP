# run historical simulations -----------------------------------------
rm(list=ls()) #remove previous variable assignments

# load packages
library(deSolve)

# load model
source("Codes/SEI-SEIR_model_TR.R")

# load data 
source("Codes/SEI-SEIR_simulation_setup.R")

# load historical climate data
load("Kenya_historical/combinedTR.RData")

# reformat climate and site info
climateData <- combinedTR
counties <- unique(combinedTR$County)
popsizes <- read.csv("Kenya_historical/county_population_sizes.csv", head=T, stringsAsFactors = F)
popsizes$Population <- gsub(",", "", popsizes$Population)
popsizes$Population <- as.numeric(popsizes$Population)
popsizes$County <- gsub("Tharaka-Nithi", "Tharaka", popsizes$County)
categories <- unique(climateData$Category)

# run simulations
traitDF <- data.frame(matrix(ncol = 11, nrow = 0))
colnames(traitDF) <- c("time", "M1", "M2", "M3", "S", "E", "I", "R", "Date", "County", "Category")
traitFileName <- "Kenya_historical/historical_model_simulations.csv"
write.csv(traitDF, traitFileName, row.names = F)

for (i in 1:length(counties)){
  for (j in 1:length(categories)){
    climateData2 <- subset(climateData, County == counties[i] & Category == categories[j])
    climateData2 <- climateData2[complete.cases(climateData2),]
    temp <- climateData2$Temperature
    rain <- climateData2$Monthly_rainfall
    Rmax <- mean(rain) + 2*sd(rain)
    Date <- climateData2$Date
    H0 <- popsizes$Population[popsizes$County==counties[i]]
    city <- counties[i]
    BR <- BRs[1]
    DR <- DRs[1]
    times <- seq(1,length(Date), by=1)
    M0 <- K_tr(temp[1], rain[1], Rmax, H0)
    parameters <- c(EFD, pEA, MDR, K_tr, a, pMI, mu_t, PDR, b, timestep=timestep)
    state <- c(M1 = startIC$m1*M0, M2 = startIC$m2*M0, M3 = startIC$m3*M0, S = startIC$s*H0, E = startIC$e*H0, I = startIC$i*H0, R = startIC$r*H0)
    out <- ode(y = state, times = times, func = seiseir_model_tr, parms = parameters, method="rk4", atol = 1e-14, rtol = 1e-14, hini = timestep)
    out2 <- as.data.frame(out)
    out2$Date <- Date
    out2$County <- counties[i]
    out2$Category <- categories[j]
    traitDF <- rbind(traitDF, out2)
    write.csv(traitDF, traitFileName, row.names = F)
    cat("finished running ode for", counties[i], categories[j], "\n")
  }
} 

datesToKeep <- c(seq.Date(as.Date("1993-01-01"), as.Date("1993-12-31"), 1),
                 seq.Date(as.Date("1999-01-01"), as.Date("1999-12-31"), 1),
                 seq.Date(as.Date("2015-01-01"), as.Date("2015-12-31"), 1))
traitDFsub <- traitDF[(traitDF$Date %in% datesToKeep),]
traitDFsub <- unique(traitDFsub)
write.csv(traitDFsub, "C:/Users/Jeremy/shiny/historical_model_simulations.csv", row.names=F)
