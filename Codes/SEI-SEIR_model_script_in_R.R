# This is a struct for the implementation of the SEI-SEIR model.
# Parameter values are defined as follows:
# x[0] = M1; x[1] = M2; x[2] = M3; x[3] = S; x[4] = E; x[5] = I; x[6] = R;
# dxdt[0] = dM1; dxdt[1] = dM2; dxdt[2] = dM3; dxdt[3] = dS; dxdt[4] = dE;
# dxdt[5] = dI; dxdt[6] = dR;
# For more documentation, please see Huber et al

# SEI-SEIR Aedes aegypti simulation model: temperature dependence only
seiseir_model_t <- function(t, state, parameters) {
  with(as.list(c(state,parameters)), {
    dM1 <- (EFD(temp[t])*pEA(temp[t])*MDR(temp[t])*mu_t(temp[t])^(-1))*(M1+M2+M3)*max((1-((M1+M2+M3)/K_t(temp[t]))),0)-(a(temp[t])*pMI(temp[t])*I/(S+E+I+R)+mu_t(temp[t]))*M1
    dM2 <- (a(temp[t])*pMI(temp[t])*I/(S+E+I+R))*M1-(PDR(temp[t])+mu_t(temp[t]))*M2
    dM3 <- PDR(temp[t])*M2-mu_t(temp[t])*M3
    dS <- -a(temp[t])*b(temp[t])*(M3/(M1+M2+M3+0.001))*.5*S + 23.9*(S/1000)/360 - 5.8*(S/1000)/360
    dE <- a(temp[t])*b(temp[t])*(M3/(M1+M2+M3+0.001))*S-(1.0/5.9)*E - 5.8*(E/1000)/360
    dI <- (1.0/5.9)*E-(1.0/5.0)*I - 5.8*(I/1000)/360
    dR <- (1.0/5.0)*I - 5.8*(R/1000)/360
    list(c(dM1, dM2, dM3, dS, dE, dI, dR))
  })
}    

# SEI-SEIR Aedes aegypti simulation model: temperature & humidity dependence
seiseir_model_th <- function(t, state, parameters) {
  with(as.list(c(state,parameters)), {
    dM1 <- (EFD(temp[t])*pEA(temp[t])*MDR(temp[t])*mu_th(temp[t], hum[t])^(-1))*(M1+M2+M3)*max((1-((M1+M2+M3)/K_th(temp[t], hum[t]))),0)-(a(temp[t])*pMI(temp[t])*I/(S+E+I+R)+mu_th(temp[t], hum[t])*M1)
    dM2 <- (a(temp[t])*pMI(temp[t])*I/(S+E+I+R))*M1-(PDR(temp[t])+mu_th(temp[t], hum[t]))*M2
    dM3 <- PDR(temp[t])*M2-mu_th(temp[t], hum[t])*M3
    dS <- -a(temp[t])*b(temp[t])*(M3/(M1+M2+M3+0.001))*S + 23.9*(S/1000)/360 - 5.8*(S/1000)/360
    dE <- a(temp[t])*b(temp[t])*(M3/(M1+M2+M3+0.001))*S-(1.0/5.9)*E - 5.8*(E/1000)/360
    dI <- (1.0/5.9)*E-(1.0/5.0)*I - 5.8*(I/1000)/360
    dR <- (1.0/5.0)*I - 5.8*(R/1000)/360
    list(c(dM1, dM2, dM3, dS, dE, dI, dR))
  })
}    


# SEI-SEIR Aedes aegypti simulation model: temperature & humidity dependence
seiseir_model_thr <- function(t, state, parameters) {
  with(as.list(c(state,parameters)), {
    # if (times[t] <= 700)
    #   R.lag <- 0
    # else
    #   R.lag <- lagvalue(times[t]-700, 6)  # returns R value 900 time steps earlier (state var # 7)
    dM1 <- (EFD(temp[t])*pEA(temp[t])*MDR(temp[t])*mu_th(temp[t], hum[t])^(-1))*(M1+M2+M3)*max((1-((M1+M2+M3)/K_thr(temp[t], hum[t], rain[t]))),0)-(a(temp[t])*pMI(temp[t])*I/(S+E+I+R)+mu_th(temp[t], hum[t])*M1)
    dM2 <- (a(temp[t])*pMI(temp[t])*I/(S+E+I+R))*M1-(PDR(temp[t])+mu_th(temp[t], hum[t]))*M2
    dM3 <- PDR(temp[t])*M2-mu_th(temp[t], hum[t])*M3
    dS <- -a(temp[t])*b(temp[t])*(M3/(M1+M2+M3+0.001))*S + 23.9*(S/1000)/360 - 5.8*(S/1000)/360 + ((0.8*R)/900) #0.16*R.lag
    dE <- a(temp[t])*b(temp[t])*(M3/(M1+M2+M3+0.001))*S-(1.0/5.9)*E - 5.8*(E/1000)/360 
    dI <- (1.0/5.9)*E-(1.0/5.0)*I - 5.8*(I/1000)/360 
    dR <- (1.0/5.0)*I - 5.8*(R/1000)/360 - ((0.8*R)/900) # 0.16*R.lag
    list(c(dM1, dM2, dM3, dS, dE, dI, dR)) #, "R.lag"=R.lag
  })
}    


# SEI-SEIR Aedes aegypti simulation model: temperature & humidity dependence AND 
# includes transovarial transmission and partial immunity in the human population
seiseir_model_thr_trans <- function(t, state, parameters) {
  with(as.list(c(state,parameters)), {
    dM1 <- 0.96*(EFD(temp[t])*pEA(temp[t])*MDR(temp[t])*mu_th(temp[t], hum[t])^(-1))*(M1+M2+M3)*max((1-((M1+M2+M3)/K_thr(temp[t], hum[t], rain[t]))),0)-(a(temp[t])*pMI(temp[t])*I/(S+E+I+R)+mu_th(temp[t], hum[t])*M1)
    dM2 <- (a(temp[t])*pMI(temp[t])*I/(S+E+I+R))*M1-(PDR(temp[t])+mu_th(temp[t], hum[t]))*M2 + 0.04*(EFD(temp[t])*pEA(temp[t])*MDR(temp[t])*mu_th(temp[t], hum[t])^(-1))*(M1+M2+M3)*max((1-((M1+M2+M3)/K_thr(temp[t], hum[t], rain[t]))),0) 
    dM3 <- PDR(temp[t])*M2-mu_th(temp[t], hum[t])*M3
    dS <- -a(temp[t])*b(temp[t])*(M3/(M1+M2+M3+0.001))*(0.2*S) + 23.9*(S/1000)/360 - 5.8*(S/1000)/360 + R/360
    dE <- a(temp[t])*b(temp[t])*(M3/(M1+M2+M3+0.001))*(0.2*S)-(1.0/5.9)*E - 5.8*(E/1000)/360
    dI <- (1.0/5.9)*E-(1.0/5.0)*I - 5.8*(I/1000)/360
    dR <- (1.0/5.0)*I - 5.8*(R/1000)/360 - R/360
    list(c(dM1, dM2, dM3, dS, dE, dI, dR))
  })
}    

# seiseir_constant_model <- function(t, state, parameters) {
#   with(as.list(c(state,parameters)), {
#     dM1 <- (EFD(temp)*pEA(temp)*MDR(temp))*(M1+M2+M3)*(1-((M1+M2+M3)/K(temp)))-(a(temp)*pMI(temp)*I/(S+E+I+R)+mu(temp))*M1
#     dM2 <- (a(temp)*pMI(temp)*I/(S+E+I+R))*M1-(PDR(temp)+mu(temp))*M2
#     dM3 <- PDR(temp)*M2-mu(temp)*M3
#     dS <- -a(temp)*b(temp)*(M3/(M1+M2+M3+0.001))*S
#     dE <- a(temp)*b(temp)*(M3/(M1+M2+M3+0.001))*S-(1.0/5.9)*E
#     dI <- (1.0/5.9)*E-(1.0/5.0)*I
#     dR <- (1.0/5.0)*I
#     list(c(dM1, dM2, dM3, dS, dE, dI, dR))
#   })
# }

# This is the general function for the Briere fit.
# For more documentation, see temperaturesensitivefunctions.hpp

briere <- function(x, c, T0, Tm){
  if((x < T0) | (x > Tm))
    0.0
  else
    c*x*(x-T0)*sqrt(Tm-x)
}

# This is the general function for the quadratic fit. 
# For more documentation, see temperaturesensitivefunctions.hpp

quadratic <- function(x, c, T0, Tm){
  if((x < T0) | (x > Tm))
    0.0
  else
    c*(x-T0)*(x-Tm)
}

# This is the general function for the inverted quadratic fit.
# For more documentation, see temperaturesensitivefunctions.hpp

inverted_quadratic <- function(x, c, T0, Tm){
  if((x < T0) | (x > Tm))
    24.0
  else
    1.0/(c*(x-T0)*(x-Tm))
}

# These are the temperature-senstive entomological parameters for the
# Ae. aegypti vector. Further documentation regarding each function is found in
# the temperaturesensitivefunctions.hpp file

# eggs per female per day
EFD <- function(temp){
  briere(temp,8.56e-03,14.58,34.61)
}

# probability egg to adult survival
pEA <- function(temp){
  quadratic(temp,-5.99e-03,13.56,38.29)
}

# mosquito development rate (1/larval development period)
MDR <- function(temp){
  briere(temp,7.86e-05,11.36,39.17)
}

# MDR <- function(temp, rain){
#   if (rain == 0){
#     briere(temp,7.86e-05,11.36,39.17)*0.0
#   } else {
#     briere(temp,7.86e-05,11.36,39.17)
#   }
# }

# biting rate
a <- function(temp){
  briere(temp,2.02e-04,13.35,40.08)
}

# probability	of mosquito	infection per	bite	on	an	infectious	host
pMI <- function(temp){
  briere(temp,4.91e-04,12.22,37.46)
}

# adult mosquito mortality rate (1/adult lifespan)
mu_t <- function(temp){
  inverted_quadratic(temp,-1.48e-01,9.16,37.73) # scaling helps for Ukunda *1.48
}

mu_th <- function(temp, hum){
  inverted_quadratic(temp,-1.48e-01,9.16,37.73+0.643*(hum/100))
}

# parasite development rate
PDR <- function(temp){
  briere(temp,6.56e-05,10.68,45.90)
}

# transmission competence: probability of human	infection	per	bite	by	an	infectious mosquito
b <- function(temp){
  briere(temp,8.49e-04,17.05,35.83)
}

# carrying capacity for temperature only
carrying_capacity_t <- function(temp, T0, EA, N){
  kappa <- 8.617e-05; # Boltzmann constant 
  alpha <- (EFD(T0)*pEA(T0)*MDR(T0)*mu_t(T0)^(-1)-mu_t(T0))/(EFD(T0)*pEA(T0)*MDR(T0)*mu_t(T0)^(-1))
  (alpha*N*exp(-EA*((temp-T0)^2)/(kappa*(temp+273.0)*(T0+273.0))))
}

carrying_capacity_th <- function(temp, T0, EA, N, h0){
  kappa <- 8.617e-05; # Boltzmann constant 
  alpha <- (EFD(T0)*pEA(T0)*MDR(T0)*mu_th(T0, h0)^(-1)-mu_th(T0, h0))/(EFD(T0)*pEA(T0)*MDR(T0)*mu_th(T0, h0)^(-1))
  (alpha*N*exp(-EA*((temp-T0)^2)/(kappa*(temp+273.0)*(T0+273.0))))
}

K_t <- function(temp){
  carrying_capacity_t(temp,29.0,0.05,20000)
}

K_th <- function(temp, hum){
  carrying_capacity_th(temp,29.0,0.05,20000, hum)
}

K_thr <- function(temp, hum, rain){
  if(rain >= 150){
    0.1*K_th(temp, hum)
  }
  else {
    # (1.267876e-05*rain*(rain-0)*sqrt(150-rain))*K_th(temp, hum) # briere
    K_th(temp, hum)*(rain/150) # linear
    # K_th(temp, hum)*exp(rain) # exponential
  }
}
