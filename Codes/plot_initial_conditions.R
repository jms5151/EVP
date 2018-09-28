# Visually compare model output given different initial conditions ------------------------

# load libraries --------------------------------------------------------------------------
library(ggplot2)

# load data -------------------------------------------------------------------------------
inits <- read.csv("Kenya/Concatenated_Data/SEI-SEIR/Initital_conditions/Initital_condition_simulations2.csv", head=T)
startDateInits <- read.csv("Kenya/Concatenated_Data/SEI-SEIR/Initital_conditions/Initital_condition_simulations_diff_start_dates.csv", head=T)

# adjust dates and ic classes
inits$Date <- as.Date(inits$Date, "%Y-%m-%d")
inits$IC <- as.factor(inits$IC)

startDateInits$Date <- as.Date(startDateInits$Date, "%Y-%m-%d")

# plot  ----------------------------------------------------------------------------------
# ICs
ics <- subset(inits, Date > "2014-01-01") # Chulaimbo takes almost two years to initialize
ggplot(ics,aes(x=Date,y=I,colour=IC,group=IC)) + geom_line() + 
  theme_bw() + facet_wrap( ~ Site, ncol=1, scales="free_y")

# start dates
ggplot(startDateInits,aes(x=Date,y=I,colour=StartDate,group=StartDate)) + geom_line() + 
  theme_bw() + facet_wrap( ~ Site, ncol=1, scales="free_y")

sd.ics <- subset(startDateInits, Date > "2015-01-01" ) # zoom in on trajectories at later time point 
ggplot(sd.ics,aes(x=Date,y=I,colour=StartDate,group=StartDate)) + geom_line() + 
  theme_bw() + facet_wrap( ~ Site, ncol=1, scales="free_y")
