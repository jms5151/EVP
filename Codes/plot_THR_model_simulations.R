# merge and plot THR SEI-SEIR model simulation data ------------------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load library
library(ggplot2)

# load data
Tmodel <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_temperature.csv", head=T, stringsAsFactors = F)
Hmodel <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_humidity.csv", head=T, stringsAsFactors = F)
Rmodel <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_rainfall.csv", head=T, stringsAsFactors = F)
THRmodel <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_THR.csv", head=T, stringsAsFactors = F)

# add treatment 
Tmodel$Treatment <- "T"
Hmodel$Treatment <- "H"
Rmodel$Treatment <- "R"
THRmodel$Treatment <- "THR"

# combine data
mods <- do.call("rbind", list(Tmodel, Hmodel, Rmodel, THRmodel))

# sum mosquitoes
mods$totM <- mods$M1 + mods$M2 + mods$M3

# format date
mods$Date <- as.Date(mods$Date, "%Y-%m-%d")

# remove first six months of simulations
mods2 <- subset(mods, time > 180)

# plot data
ggplot(data=mods2, aes(x=Date, y=I, group=Treatment, colour=Treatment)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Site, scales = "free", ncol=4) +
  theme_bw()

ggplot(data=mods2, aes(x=Date, y=totM, group=Treatment, colour=Treatment)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Site, scales = "free", ncol=4) +
  theme_bw()
