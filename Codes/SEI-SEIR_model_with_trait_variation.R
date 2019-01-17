# SEI-SEIR model with temperature, humidity, and rainfall --------------------------------
seiseir_model_thr <- function(t, state, parameters) {
  with(as.list(c(state,parameters)), {
    dM1 <- (EFD(temp[t])*pEA(temp[t])*MDR(temp[t])*mu_th(temp[t], hum[t])^(-1))*(M1+M2+M3)*max((1-((M1+M2+M3)/K_thr(temp[t], hum[t], rain[t]))),0)-(a(temp[t])*pMI(temp[t])*I/(S+E+I+R)+mu_th(temp[t], hum[t])*M1)
    dM2 <- (a(temp[t])*pMI(temp[t])*I/(S+E+I+R))*M1-(PDR(temp[t])+mu_th(temp[t], hum[t]))*M2
    dM3 <- PDR(temp[t])*M2-mu_th(temp[t], hum[t])*M3
    dS <- -a(temp[t])*b(temp[t])*(M3/(M1+M2+M3+0.001))*S + BR*(S/1000)/360 - DR*(S/1000)/360 + ie*(S+E+I+R) - ie*S
    dE <- a(temp[t])*b(temp[t])*(M3/(M1+M2+M3+0.001))*S-(1.0/5.9)*E - DR*(E/1000)/360 - ie*E
    dI <- (1.0/5.9)*E-(1.0/5.0)*I - DR*(I/1000)/360 - ie*I
    dR <- (1.0/5.0)*I - DR*(R/1000)/360 - ie*R
    list(c(dM1, dM2, dM3, dS, dE, dI, dR))
  })
}    

# This is the general function for the Briere fit.
briere <- function(x, c, T0, Tm){
  if((x < T0) | (x > Tm))
    0.0
  else
    c*x*(x-T0)*sqrt(Tm-x)
}

# This is the general function for the quadratic fit. 
quadratic <- function(x, c, T0, Tm){
  if((x < T0) | (x > Tm))
    0.0
  else
    c*(x-T0)*(x-Tm)
}

# This is the general function for the inverted quadratic fit.
inverted_quadratic <- function(x, c, T0, Tm){
  if((x < T0) | (x > Tm))
    24.0
  else
    1.0/(c*(x-T0)*(x-Tm))
}

# eggs per female per day
EFD <- function(temp){
  # briere(temp,8.56e-03,14.58,34.61)
  briere(temp, EFD_c, EFD_T0, EFD_Tm)
}

# probability egg to adult survival
pEA <- function(temp){
  # quadratic(temp,-5.99e-03,13.56,38.29)
  quadratic(temp, pEA_c, pEA_T0, pEA_Tm)
}

# mosquito development rate (1/larval development period)
MDR <- function(temp){
  # briere(temp,7.86e-05,11.36,39.17)
  briere(temp, MDR_c, MDR_T0, MDR_Tm)
}

# biting rate
a <- function(temp){
  # briere(temp,2.02e-04,13.35,40.08)
  briere(temp, a_c, a_T0, a_Tm)
}

# probability	of mosquito	infection per	bite	on	an	infectious	host
pMI <- function(temp){
  # briere(temp,4.91e-04,12.22,37.46)
  briere(temp, pMI_c, pMI_T0, pMI_Tm)
}

# adult mosquito mortality rate (1/adult lifespan)
mu_th <- function(temp, hum){
  # inverted_quadratic(temp,-1.48e-01,9.16,37.73+0.643*(hum/100))
  inverted_quadratic(temp, mu_th_c, mu_th_T0, mu_th_Tm + 0.643*(hum/100))
}

# parasite development rate
PDR <- function(temp){
  # briere(temp,6.56e-05,10.68,45.90)
  briere(temp, PDR_c, PDR_T0, PDR_Tm)
}

# transmission competence: probability of human	infection	per	bite	by	an	infectious mosquito
b <- function(temp){
  # briere(temp,8.49e-04,17.05,35.83)
  briere(temp, b_c, b_T0, b_Tm)
}

# carrying capacity for temperature only
carrying_capacity_th <- function(temp, T0, EA, N, h0){
  kappa <- 8.617e-05; # Boltzmann constant 
  alpha <- (EFD(T0)*pEA(T0)*MDR(T0)*mu_th(T0, h0)^(-1)-mu_th(T0, h0))/(EFD(T0)*pEA(T0)*MDR(T0)*mu_th(T0, h0)^(-1))
  (alpha*N*exp(-EA*((temp-T0)^2)/(kappa*(temp+273.0)*(T0+273.0))))
}

K_thr <- function(temp, hum, rain){
  if(rain >= 55){
    0.1*carrying_capacity_th(temp,29.0,0.05,20000, hum)
  }
  else {
    # (1.267876e-05*rain*(rain-0)*sqrt(150-rain))*K_th(temp, hum) # briere
    carrying_capacity_th(temp,29.0,0.05,20000, hum)*(rain/55) # linear
    # K_th(temp, hum)*exp(rain) # exponential
  }
}
