# biting rate with temperature and humidity
a_none <- function(temp, hum){
  briere(temp,2.02e-04,13.35,40.08)
}

a_linear <- function(temp, hum){
  briere(temp,2.02e-04,13.35,40.08)*(0.643*(hum/100)+0.4)
}

a_sigmoidal <- function(temp, hum){
  briere(temp,2.02e-04,13.35,40.08)*(1/(1+exp(-hum + 40)))
}

# mortality rate with temperature and humidity
mu_th_linear <- function(temp, hum){
  min(inverted_quadratic(temp,-1.48e-01, 9.16, 37.73)+(1-((0.643*(hum/100)+0.4))), 23)
}

mu_th_sigmoidal <- function(temp, hum){
  min(inverted_quadratic(temp,-1.48e-01, 9.16, 37.73)+(1-(1/(1+exp(-hum + 40)))), 23)
}

# carrying capacity with temperature, humidity, and rainfall
K_thr_right_skewed <- function(temp, hum, rain, Rmax, N){
  R0 <- 1
  if((rain < R0) | (rain > Rmax)){
    0.01*carrying_capacity_th(temp, 29.0, 0.05, N, hum)
  }
  else {
    c <- -5.99e-03
    carrying_capacity_th(temp,29.0,0.05, N, hum)*(c*(rain-R0)*(rain-Rmax))/(rain/4) + 0.001
  }
}

K_thr_briere <- function(temp, hum, rain, Rmax, N){
  R0 <- 1
  if((rain < R0) | (rain > Rmax)){
    0.01*carrying_capacity_th(temp, 29.0, 0.05, N, hum)
  }
  else {
    c <- 7.86e-05
    carrying_capacity_th(temp,29.0,0.05, N, hum)*c*rain*(rain-R0)*sqrt(Rmax-rain)*2 + 0.001
  }
}

K_thr_quadratic <- function(temp, hum, rain, Rmax, N){
  R0 <- 1
  if((rain < R0) | (rain > Rmax)){
    0.01*carrying_capacity_th(temp, 29.0, 0.05, N, hum)
  }
  else {
    c <- -5.99e-03
    carrying_capacity_th(temp,29.0,0.05, N, hum)*(c*(rain-R0)*(rain-Rmax))/abs((c*(Rmax/2-R0)*(Rmax/2-Rmax))) + 0.001
  }
}

