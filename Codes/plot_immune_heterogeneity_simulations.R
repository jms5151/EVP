# Visually compare model output given different immune heterogeneity ----------------------

# load libraries 
library(ggplot2)

# load data 
immune.hetero <- read.csv("Concatenated_Data/sensitivity_analyses/SEI-SEIR_simulations_with_immune_heterogeneity_E.csv", head=T)
# immune.hetero <- read.csv("Concatenated_Data/sensitivity_analyses/SEI-SEIR_simulations_with_immune_heterogeneity_I.csv", head=T)
immune.hetero <- read.csv("Concatenated_Data/sensitivity_analyses/SEI-SEIR_simulations_with_immune_heterogeneity_ie.csv", head=T)

# format dates and simulation number
immune.hetero$Date <- as.Date(immune.hetero$Date, "%Y-%m-%d")
# immune.hetero$sim <- as.factor(paste0("S-", immune.hetero$immS, ":U-", immune.hetero$immU))
immune.hetero$ie <- as.factor(immune.hetero$ie)

# plot  ----------------------------------------------------------------------------------
# test<-subset(immune.hetero, Date > "2016-01-01")
# test<-subset(immune.hetero, Site == "Huaquillas" & ie == "0.01")
# test<-subset(immune.hetero, immU == "0.1")
ggplot(immune.hetero, aes(x=Date, y=I, colour=ie)) + geom_line() + 
  theme_bw() + facet_wrap( ~ Site, ncol=2, scales="free")
plot(test$Date, test$M, ylim=c(0,500), type='l')
