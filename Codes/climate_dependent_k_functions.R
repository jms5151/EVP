# humidity-dependent mosquito carrying capacity 
K_hum <- function(H){
  ((0.643*H)-(0.00850*(25-34.9)*(25-2.21)))/66.21778 # scaled between 0.03 and 1
}

# rainfall-dependent mosquito carrying capacity functions to test 
K_rain_linear <- function(R){
  # R/700 # divide by max 1 month rainfall accumulation (700 mm) to scale between 0 and 1   
  R/500
}

K_rain_hump <- function(R){
  -((((R+0.0001)-0.0001)*(R-500))/62500) # divide by least optimal rainfall and add 1 to scale between 0 and 1 
}

# K_rain_invert_hump <- function(R){
#   (((R-0.0001)*(R-700))/84680.97)+1 # divide by least optimal rainfall and add 1 to scale between 0 and 1 
# }

K_rain_briere <- function(R){
  ((R*((R+0.0001)-0.0001))*sqrt(500-R))/1600000 # divide by optimal rainfall to scale between 0 and 1
}

# temperature-dependent mosquito carrying capacity function
K_temp <- function(T){
  ((EFD(T)*pEA(T)*MDR(T))/mu(T)^2)/671.7754 # divide by max number of mosquitoes to scale between 0 and 1
}

# briere function
briere <- function(x, c, T0, Tm){
  if((x < T0) | (x > Tm))
    0.0
  else
    c*x*(x-T0)*sqrt(Tm-x)
}

# quadratic function
quadratic <- function(x, c, T0, Tm){
  if((x < T0) | (x > Tm))
    0.0
  else
    c*(x-T0)*(x-Tm)
}

# inverted quadratic function
inverted_quadratic <- function(x, c, T0, Tm){
  if((x < T0) | (x > Tm))
    24.0
  else
    1.0/(c*(x-T0)*(x-Tm))
}

# eggs per female per day
EFD <- function(temp){
  briere(temp,8.56e-03,14.58,34.61)
  #  briere(temp,2.08e-02,29,41)
}

# probability egg to adult survival
pEA <- function(temp){
  quadratic(temp,-5.99e-03,13.56,38.29)
}

# mosquito development rate (1/larval development period)
MDR <- function(temp){
  briere(temp,7.86e-05,11.36,39.17)
}

# adult mosquito mortality rate (1/adult lifespan)
mu <- function(temp){
  inverted_quadratic(temp,-1.48e-01,9.16,37.73)
}

# plot relationships -------------------------------------------
# temperature
# tempSeq <- seq(12,40,.2)
# temp <- c()
# for (i in 1:length(tempSeq)){
#   temp <- c(temp, K_temp(tempSeq[i]))
# }
# plot(tempSeq, temp, type='l', lwd=2, ylab=c("K (scaled)"), xlab=expression(paste("Temperature (",degree,"C)")), col="red", main="Chulaimbo")
# 
# # # humidity
# humSeq <- seq(0,100,1)
# hum <- c()
# for (i in 1:length(humSeq)){
#   hum <- c(hum, K_hum(humSeq[i]))
# }
# plot(humSeq, hum, type='l', lwd=2, ylab=c("Mosquito carrying capacity (scaled)"), xlab=c("Relative humidity"), col="orange")
# 
# K_rain_briere <- function(R){
#   1.267876e-05*((R*((R+0.0001)-0.0001))*sqrt(150-R)) # divide by optimal rainfall to scale between 0 and 1
# }

# K_rain_exp <- function(R){
#   (R)^2/22500 # divide by least optimal rainfall and add 1 to scale between 0 and 1 
# }
# rainSeq <- seq(0,150,1)
# r.briere <- c()
# # r.hump <- c()
# # r.linear <- c()
# r.exp <- c()
# for (i in 1:length(rainSeq)){
#   r.exp <- c(r.exp, K_rain_exp(rainSeq[i]))
# r.briere <- c(r.briere, K_rain_briere(rainSeq[i]))
# r.hump <- c(r.hump, K_rain_hump(rainSeq[i]))
# r.linear <- c(r.linear, K_rain_linear(rainSeq[i]))
# }
# min(r.briere)
# max(r.briere)
# # 
# plot(rainSeq, r.exp, type='l', lwd=2, ylab=c("Mosquito carrying capacity (scaled)"), xlab=c("Cumulative rainfall (mm)"), col="cadetblue")
# plot(rainSeq, r.briere, type='l', lwd=2, ylab=c("Mosquito carrying capacity (scaled)"), xlab=c("Cumulative rainfall (mm)"), col="cadetblue")
# plot(rainSeq, r.hump, type='l', lwd=2, ylab=c("Mosquito carrying capacity (scaled)"), xlab=c("Cumulative rainfall (mm)"), col="cadetblue")
# plot(rainSeq, r.linear, type='l', lwd=2, ylab=c("Mosquito carrying capacity (scaled)"), xlab=c("Cumulative rainfall (mm)"), col="cadetblue")