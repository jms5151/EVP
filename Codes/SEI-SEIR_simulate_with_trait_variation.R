# SEI-SEIR model simulations with trait variability --------------------------------
rm(list=ls()) #remove previous variable assignments

# load packages
library(deSolve)

# load data
source("Codes/SEI-SEIR_model_with_trait_variation.R")

a_posterior <- read.csv("trait_fits/biting_rate.csv", head=T)
pEA_posterior <- read.csv("trait_fits/egg_to_adult_survival_probability.csv", head=T)
EFD_posterior <- read.csv("trait_fits/eggs_per_female_per_day.csv", head=T)
mu_posterior <- read.csv("trait_fits/lifespan.csv", head=T)
MDR_posterior <- read.csv("trait_fits/mosquito_developement_rate.csv", head=T)
PDR_posterior <- read.csv("trait_fits/parasite_development_rate.csv", head=T)
b_posterior <- read.csv("trait_fits/vector_competence_b.csv", head=T)
pMI_posterior <- read.csv("trait_fits/vector_competence_c.csv", head=T)

climateData <- read.csv("Kenya/Concatenated_Data/climate/gapfilled_climate_data.csv", head=T)
climateData$Date <- as.Date(climateData$Date, "%Y-%m-%d")

# randomly select rows of posterior distributions ---------------------------------
posteriors <- list(a_posterior, pEA_posterior, EFD_posterior, mu_posterior, MDR_posterior, PDR_posterior, b_posterior, pMI_posterior)
traitNames <- c("a", "pEA", "EFD", "mu_th", "MDR", "PDR", "b", "pMI")
n = 50 # number of samples
trait_posterior <- data.frame("ID"=seq(1:n))

for (i in 1:length(posteriors)){
  trait.sub <- posteriors[[i]]
  trait.sub <- trait.sub[sample(nrow(trait.sub), n), ]
  if (traitNames[i] == "pEA"|traitNames[i] == "mu_th"){
    colnames(trait.sub)[3] <- "c"
  }
  colnames(trait.sub) <- paste0(traitNames[i], "_", colnames(trait.sub))
  trait_posterior <- cbind(trait_posterior, trait.sub[,1:3])
}

write.csv(trait_posterior, "Kenya/Concatenated_Data/SEI-SEIR/Random_sample_of_posterior_traits.csv", row.names = F)

# select initial conditions for future simulations -------------------------------------------
inits <- read.csv("Kenya/Concatenated_Data/SEI-SEIR/Initital_conditions/LHS_inputs.csv", head=T)
startIC <- subset(inits, IC == "18")

# run simulations -----------------------------------------------------------------
population <- c(5336, 419072,14444,75357)
sites <- c("Chulaimbo", "Kisumu", "Msambweni", "Ukunda")
timestep = 1/12
traitDF <- data.frame(matrix(ncol = 11, nrow = 0))
colnames(traitDF) <- c("time", "M1", "M2", "M3", "S", "E", "I", "R", "Date", "simulation_number", "Site")  
traitFileName <- "Kenya/Concatenated_Data/SEI-SEIR/SEI-SEIR_simulations_with_trait_variation.csv"
write.csv(traitDF, traitFileName, row.names = F)

for (j in 1:nrow(trait_posterior)){
  for (k in 2:ncol(trait_posterior)){
    x <- colnames(trait_posterior)[k]
    y <- trait_posterior[j,k]
    assign(x,y)
  }
  for (l in 1:length(sites)){
    # adjust by date if needed climateData <- subset(climateData, Date >= )
    t_colName <- paste0("GF_", sites[l], "_mean_temp")
    temp <- climateData[,t_colName]
    h_colName <- paste0("GF_", sites[l], "_humidity")
    hum <- climateData[,h_colName]
    r_colName <- paste0("GF_", sites[l], "_cumRain")
    rain <- climateData[,r_colName]
    M0 <- K_thr(temp[1], hum[1], rain[1])
    Date <- climateData$Date
    H0 <- population[j]
    city <- sites[j]
    times <- seq(1,length(Date), by=1)
    parameters <- c(EFD, pEA, MDR, K_thr, a, pMI, mu_th, PDR, b, timestep=timestep)
    state <- c(M1 = startIC$m1*M0, M2 = startIC$m2*M0, M3 = startIC$m3*M0, S = startIC$s*H0, E = startIC$e*H0, I = startIC$i*H0, R = startIC$r*H0)
    out <- ode(y = state, times = times, func = seiseir_model_thr, parms = parameters, method="rk4", atol = 1e-14, rtol = 1e-14, hini = timestep)
    out2 <- as.data.frame(out)
    out2$Date <- Date
    out2$simulation_number <- j
    out2$Site <- sites[j]
    traitDF <- rbind(traitDF, out2)
    write.csv(traitDF, traitFileName, row.names = F)
    cat("finished running ode for", sites[l], "simulation #", j, "\n")
  }
}
