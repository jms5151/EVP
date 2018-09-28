rm(list=ls()) #remove previous variable assignments
source("Codes/climate_dependent_k_functions.R")

# temperature
tSeq <- seq(12,45,.25)
temps <- c()

for (i in 1:length(tSeq)){
  temps <- c(temps, K_temp(tSeq[i]))
}

plot(tSeq, temps, type="l", lty=1, ylab = c("Transmission suitability"), xlab = c("Temperature (C)"))

# rainfall
rSeq <- seq(0,700,1)
rain_b <- c()
rain_h <- c()
rain_ih <- c()
rain_lin <- c()

for (i in 1:length(rSeq)){
  rain_b <- c(rain_b, K_rain_briere(rSeq[i]))
  rain_h <- c(rain_h, K_rain_hump(rSeq[i]))
  rain_ih <- c(rain_ih, K_rain_invert_hump(rSeq[i]))
  rain_lin <- c(rain_lin, K_rain_linear(rSeq[i]))
}

plot(rSeq, rain_b, type="l", lty=1, ylab = c("Transmission suitability"), xlab = c("Total rain (mm)"))
plot(rSeq, rain_h, type="l", lty=1, ylab = c("Transmission suitability"), xlab = c("Total rain (mm)"))
plot(rSeq, rain_ih, type="l", lty=1, ylab = c("Transmission suitability"), xlab = c("Total rain (mm)"))
plot(rSeq, rain_lin, type="l", lty=1, ylab = c("Transmission suitability"), xlab = c("Total rain (mm)"))

# humidity
hSeq <- seq(0,100,1)
humid <- c()

for (i in 1:length(hSeq)){
  humid <- c(humid, K_hum(hSeq[i]))
}

plot(hSeq, humid, type="l", lty=1, ylab = c("Transmission suitability"), xlab = c("Relative humidity"))

