# K functions -----------------------------------------------------------------------------------------------------
K_thr_linear_decreasing <- function(temp, hum, rain, Rmax, N){
  if(rain >= Rmax){
    0.01*K_th(temp, hum, N)
  }
  else {
    K_th(temp, hum, N)*(-(rain+0.001)/Rmax+1)
  }
}

K_thr_linear_increasing <- function(temp, hum, rain, Rmax, N){
  if(rain >= Rmax){
    0.01*K_th(temp, hum, N)
  }
  else {
    K_th(temp, hum, N)*((rain+0.001)/Rmax)
  }
}

K_thr_exponential_decreasing <- function(temp, hum, rain, Rmax, N){
    carrying_capacity_th(temp,29.0,0.05, N, hum)*exp(0.05)^(-rain)
}

K_thr_exponential_increasing <- function(temp, hum, rain, Rmax, N){
  carrying_capacity_th(temp,29.0,0.05, N, hum)*log(rain+0.001)/log(Rmax+0.001)
}

K_thr_quadratic <- function(temp, hum, rain, Rmax, N){
  R0 <- 1
  if((rain < R0) | (rain > Rmax)){
    0.01*K_th(temp, hum, N)
  }
  else {
    c <- -5.99e-03
    carrying_capacity_th(temp,29.0,0.05, N, hum)*(c*(rain-R0)*(rain-Rmax))/abs((c*(Rmax/2-R0)*(Rmax/2-Rmax))) + 0.001
  }
}

K_thr_right_skewed <- function(temp, hum, rain, Rmax, N){
  R0 <- 1
  if((rain < R0) | (rain > Rmax)){
    0.01*K_th(temp, hum, N)
  }
  else {
    c <- -5.99e-03
    carrying_capacity_th(temp,29.0,0.05, N, hum)*(c*(rain-R0)*(rain-Rmax))/(rain/4) + 0.001
  }
}

K_thr_briere <- function(temp, hum, rain, Rmax, N){
  R0 <- 1
  if((rain < R0) | (rain > Rmax)){
    0.01*K_th(temp, hum, N)
  }
  else {
    c <- 7.86e-05
    carrying_capacity_th(temp,29.0,0.05, N, hum)*c*rain*(rain-R0)*sqrt(Rmax-rain)*2 + 0.001
  }
}

# EFD functions --------------------------------------------------------------------------------------------------
EFD_linear_decreasing <- function(temp, rain, Rmax){
  if(rain >= Rmax){
    0.01*briere(temp,8.56e-03,14.58,34.61)
  }
  else {
    briere(temp,8.56e-03,14.58,34.61)*(-(rain+0.001)/Rmax+1)
  }
}

EFD_linear_increasing <- function(temp, rain, Rmax){
  if(rain >= Rmax){
    0.01*briere(temp,8.56e-03,14.58,34.61)
  }
  else {
    briere(temp,8.56e-03,14.58,34.61)*((rain+0.001)/Rmax)
  }
}

EFD_exponential_decreasing <- function(temp, rain, Rmax){
  briere(temp,8.56e-03,14.58,34.61)*exp(0.05)^(-rain)
}

EFD_exponential_increasing <- function(temp, rain, Rmax){
  briere(temp,8.56e-03,14.58,34.61)*log(rain+0.001)/log(Rmax+0.001)
}

EFD_quadratic <- function(temp, rain, Rmax){
  R0 <- 1
  if((rain < R0) | (rain > Rmax)){
    0.01*briere(temp,8.56e-03,14.58,34.61)
  }
  else {
    c <- -5.99e-03
    briere(temp,8.56e-03,14.58,34.61)*(c*(rain-R0)*(rain-Rmax))/abs((c*(Rmax/2-R0)*(Rmax/2-Rmax))) + 0.001
  }
}

EFD_right_skewed <- function(temp, rain, Rmax){
  R0 <- 1
  if((rain < R0) | (rain > Rmax)){
    0.01*briere(temp,8.56e-03,14.58,34.61)
  }
  else {
    c <- -5.99e-03
    briere(temp,8.56e-03,14.58,34.61)*(c*(rain-R0)*(rain-Rmax))/(rain/4) + 0.001
  }
}

EFD_briere <- function(temp, rain, Rmax){
  R0 <- 1
  if((rain < R0) | (rain > Rmax)){
    0.01*briere(temp,8.56e-03,14.58,34.61)
  }
  else {
    c <- 7.86e-05
    briere(temp,8.56e-03,14.58,34.61)*c*rain*(rain-R0)*sqrt(Rmax-rain)*2 + 0.001
  }
}

# pEA functions -------------------------------------------------------------------------------------------------
pEA_linear_decreasing <- function(temp, rain, Rmax){
  if(rain >= Rmax){
    0.01*quadratic(temp,-5.99e-03,13.56,38.29)
  }
  else {
    quadratic(temp,-5.99e-03,13.56,38.29)*(-(rain+0.001)/Rmax+1)
  }
}

pEA_linear_increasing <- function(temp, rain, Rmax){
  if(rain >= Rmax){
    0.01*quadratic(temp,-5.99e-03,13.56,38.29)
  }
  else {
    briere(temp,8.56e-03,14.58,34.61)*((rain+0.001)/Rmax)
  }
}

pEA_exponential_decreasing <- function(temp, rain, Rmax){
  quadratic(temp,-5.99e-03,13.56,38.29)*exp(0.05)^(-rain)
}

pEA_exponential_increasing <- function(temp, rain, Rmax){
  quadratic(temp,-5.99e-03,13.56,38.29)*log(rain+0.001)/log(Rmax+0.001)
}

pEA_quadratic <- function(temp, rain, Rmax){
  R0 <- 1
  if((rain < R0) | (rain > Rmax)){
    0.01*quadratic(temp,-5.99e-03,13.56,38.29)
  }
  else {
    c <- -5.99e-03
    quadratic(temp,-5.99e-03,13.56,38.29)*(c*(rain-R0)*(rain-Rmax))/abs((c*(Rmax/2-R0)*(Rm/2-Rmax))) + 0.001
  }
}

pEA_right_skewed <- function(temp, rain, c, R0, Rmax){
  R0 <- 1
  if((rain < R0) | (rain > Rmax)){
    0.01*quadratic(temp,-5.99e-03,13.56,38.29)
  }
  else {
    c <- -5.99e-03
    quadratic(temp,-5.99e-03,13.56,38.29)*(c*(rain-R0)*(rain-Rmax))/(rain/4) + 0.001
  }
}

pEA_briere <- function(temp, rain, Rmax){
  R0 <- 1
  if((rain < R0) | (rain > Rmax)){
    0.01*quadratic(temp,-5.99e-03,13.56,38.29)
  }
  else {
    c <- 7.86e-05
    quadratic(temp,-5.99e-03,13.56,38.29)*c*rain*(rain-R0)*sqrt(Rmax-rain)*2 + 0.001
  }
}

# MDR functions -------------------------------------------------------------------------------------------------
MDR_linear_decreasing <- function(temp, rain, Rmax){
  if(rain >= Rmax){
    0.01*briere(temp,7.86e-05,11.36,39.17)
  }
  else {
    briere(temp,7.86e-05,11.36,39.17)*(-(rain+0.001)/Rmax+1)
  }
}

MDR_linear_increasing <- function(temp, rain, Rmax){
  if(rain >= Rmax){
    0.01*briere(temp,7.86e-05,11.36,39.17)
  }
  else {
    briere(temp,7.86e-05,11.36,39.17)*((rain+0.001)/Rmax)
  }
}

MDR_exponential_decreasing <- function(temp, rain, Rmax){
  briere(temp,7.86e-05,11.36,39.17)*exp(0.05)^(-rain)
}

MDR_exponential_increasing <- function(temp, rain, Rmax){
  briere(temp,7.86e-05,11.36,39.17)*log(rain+0.001)/log(Rmax+0.001)
}

MDR_quadratic <- function(temp, rain, Rmax){
  R0 <- 1
  if((rain < R0) | (rain > Rmax)){
    0.01*briere(temp,7.86e-05,11.36,39.17)
  }
  else {
    c <- -5.99e-03
    briere(temp,7.86e-05,11.36,39.17)*(c*(rain-R0)*(rain-Rmax))/abs((c*(Rmax/2-R0)*(Rmax/2-Rmax))) + 0.001
  }
}

MDR_right_skewed <- function(temp, rain, Rmax){
  R0 <- 1
  if((rain < R0) | (rain > Rmax)){
    0.01*briere(temp,7.86e-05,11.36,39.17)
  }
  else {
    c <- -5.99e-03
    briere(temp,7.86e-05,11.36,39.17)*(c*(rain-R0)*(rain-Rmax))/(rain/4) + 0.001
  }
}

MDR_briere <- function(temp, rain, Rmax){
  R0 <- 1
  if((rain < R0) | (rain > Rmax)){
    0.01*briere(temp,7.86e-05,11.36,39.17)
  }
  else {
    c <- 7.86e-05
    briere(temp,7.86e-05,11.36,39.17)*c*rain*(rain-R0)*sqrt(Rmax-rain)*2 + 0.001
  }
}

# list functions ------------------------------------------------------------------------------------------------
r_functions <- list(K_thr_linear_increasing
                  , K_thr_linear_decreasing
                  , K_thr_exponential_increasing
                  , K_thr_exponential_decreasing
                  , K_thr_quadratic
                  , K_thr_right_skewed
                  , K_thr_briere)
                  # , EFD_linear_increasing
                  # , EFD_linear_decreasing
                  # , EFD_exponential_increasing
                  # , EFD_exponential_decreasing
                  # , EFD_quadratic
                  # , EFD_right_skewed
                  # , EFD_briere
                  # , pEA_linear_increasing
                  # , pEA_linear_decreasing
                  # , pEA_exponential_increasing
                  # , pEA_exponential_decreasing
                  # , pEA_quadratic
                  # , pEA_right_skewed
                  # , pEA_briere
                  # , MDR_linear_increasing
                  # , MDR_linear_decreasing
                  # , MDR_exponential_increasing
                  # , MDR_exponential_decreasing
                  # , MDR_quadratic
                  # , MDR_right_skewed
                  # , MDR_briere)


r_function_names <- c("K_thr_linear_increasing"
                      , "K_thr_linear_decreasing"
                      , "K_thr_exponential_increasing"
                      , "K_thr_exponential_decreasing"
                      , "K_thr_quadratic"
                      , "K_thr_right_skewed"
                      , "K_thr_briere")
                      # , "EFD_linear_increasing"
                      # , "EFD_linear_decreasing"
                      # , "EFD_exponential_increasing"
                      # , "EFD_exponential_decreasing"
                      # , "EFD_quadratic"
                      # , "EFD_right_skewed"
                      # , "EFD_briere"
                      # , "pEA_linear_increasing"
                      # , "pEA_linear_decreasing"
                      # , "pEA_exponential_increasing"
                      # , "pEA_exponential_decreasing"
                      # , "pEA_quadratic"
                      # , "pEA_right_skewed"
                      # , "pEA_briere"
                      # , "MDR_linear_increasing"
                      # , "MDR_linear_decreasing"
                      # , "MDR_exponential_increasing"
                      # , "MDR_exponential_decreasing"
                      # , "MDR_quadratic"
                      # , "MDR_right_skewed"
                      # , "MDR_briere")

# seiseir_model_thr_efd2 <- function(t, state, parameters) {
#   with(as.list(c(state,parameters)), {
#     dM1 <- (EFD(temp[t], rain[t], Rmax)*pEA(temp[t])*MDR(temp[t])*mu_th(temp[t], hum[t])^(-1))*(M1+M2+M3)*max((1-((M1+M2+M3)/K_th_efd(temp[t], hum[t], rain[t], Rmax))),0)-(a(temp[t])*pMI(temp[t])*I/(S+E+I+R)+mu_th(temp[t], hum[t])*M1)
#     dM2 <- (a(temp[t])*pMI(temp[t])*I/(S+E+I+R))*M1-(PDR(temp[t])+mu_th(temp[t], hum[t]))*M2
#     dM3 <- PDR(temp[t])*M2-mu_th(temp[t], hum[t])*M3
#     dS <- -a(temp[t])*b(temp[t])*(M3/(M1+M2+M3+0.001))*S + BR*(S/1000)/360 - DR*(S/1000)/360 + ie*(S+E+I+R) - ie*S
#     dE <- a(temp[t])*b(temp[t])*(M3/(M1+M2+M3+0.001))*S-(1.0/5.9)*E - DR*(E/1000)/360 - ie*E
#     dI <- (1.0/5.9)*E-(1.0/5.0)*I - DR*(I/1000)/360 - ie*I
#     dR <- (1.0/5.0)*I - DR*(R/1000)/360 - ie*R
#     list(c(dM1, dM2, dM3, dS, dE, dI, dR))
#   })
# }   
# 
# K_th_efd <- function(temp, hum, rain, Rmax){
#   T0 <- 29.0
#   EA <- 0.05
#   N <- 20000
#   kappa <- 8.617e-05; # Boltzmann constant 
#   alpha <- (EFD(T0, rain, Rmax)*pEA(T0)*MDR(T0)*mu_th(T0, hum)^(-1)-mu_th(T0, hum))/(EFD(T0, rain, Rmax)*pEA(T0)*MDR(T0)*mu_th(T0, hum)^(-1))
#   (alpha*N*exp(-EA*((temp-T0)^2)/(kappa*(temp+273.0)*(T0+273.0))))
# }
# 
# seiseir_model_thr_pEA2 <- function(t, state, parameters) {
#   with(as.list(c(state,parameters)), {
#     dM1 <- (EFD(temp[t])*pEA(temp[t], rain[t], Rmax)*MDR(temp[t])*mu_th(temp[t], hum[t])^(-1))*(M1+M2+M3)*max((1-((M1+M2+M3)/K_th_pEA(temp[t], hum[t], rain[t], Rmax))),0)-(a(temp[t])*pMI(temp[t])*I/(S+E+I+R)+mu_th(temp[t], hum[t])*M1)
#     dM2 <- (a(temp[t])*pMI(temp[t])*I/(S+E+I+R))*M1-(PDR(temp[t])+mu_th(temp[t], hum[t]))*M2
#     dM3 <- PDR(temp[t])*M2-mu_th(temp[t], hum[t])*M3
#     dS <- -a(temp[t])*b(temp[t])*(M3/(M1+M2+M3+0.001))*S + BR*(S/1000)/360 - DR*(S/1000)/360 + ie*(S+E+I+R) - ie*S
#     dE <- a(temp[t])*b(temp[t])*(M3/(M1+M2+M3+0.001))*S-(1.0/5.9)*E - DR*(E/1000)/360 - ie*E
#     dI <- (1.0/5.9)*E-(1.0/5.0)*I - DR*(I/1000)/360 - ie*I
#     dR <- (1.0/5.0)*I - DR*(R/1000)/360 - ie*R
#     list(c(dM1, dM2, dM3, dS, dE, dI, dR))
#   })
# }   
# 
# K_th_pEA <- function(temp, hum, rain, Rmax){
#   T0 <- 29.0
#   EA <- 0.05
#   N <- 20000
#   kappa <- 8.617e-05; # Boltzmann constant 
#   alpha <- (EFD(T0)*pEA(T0, rain, Rmax)*MDR(T0)*mu_th(T0, hum)^(-1)-mu_th(T0, hum))/(EFD(T0)*pEA(T0, rain, Rmax)*MDR(T0)*mu_th(T0, hum)^(-1))
#   (alpha*N*exp(-EA*((temp-T0)^2)/(kappa*(temp+273.0)*(T0+273.0))))
# }
# 
# seiseir_model_thr_mdr2 <- function(t, state, parameters) {
#   with(as.list(c(state,parameters)), {
#     dM1 <- (EFD(temp[t])*pEA(temp[t])*MDR(temp[t], rain[t], Rmax)*mu_th(temp[t], hum[t])^(-1))*(M1+M2+M3)*max((1-((M1+M2+M3)/K_th_MDR(temp[t], hum[t](EFD(temp[t])*pEA(temp[t], rain[t], Rmax)*MDR(temp[t])*mu_th(temp[t], hum[t])^(-1))*(M1+M2+M3)*(1-((M1+M2+M3)/K_th_pEA(temp[t], hum[t], rain[t], Rmax)))))),0)-(a(temp[t])*pMI(temp[t])*I/(S+E+I+R)+mu_th(temp[t], hum[t])*M1)
#     dM2 <- (a(temp[t])*pMI(temp[t])*I/(S+E+I+R))*M1-(PDR(temp[t])+mu_th(temp[t], hum[t]))*M2
#     dM3 <- PDR(temp[t])*M2-mu_th(temp[t], hum[t])*M3
#     dS <- -a(temp[t])*b(temp[t])*(M3/(M1+M2+M3+0.001))*S + BR*(S/1000)/360 - DR*(S/1000)/360 + ie*(S+E+I+R) - ie*S
#     dE <- a(temp[t])*b(temp[t])*(M3/(M1+M2+M3+0.001))*S-(1.0/5.9)*E - DR*(E/1000)/360 - ie*E
#     dI <- (1.0/5.9)*E-(1.0/5.0)*I - DR*(I/1000)/360 - ie*I
#     dR <- (1.0/5.0)*I - DR*(R/1000)/360 - ie*R
#     list(c(dM1, dM2, dM3, dS, dE, dI, dR))
#   })
# }   
# 
# K_th_MDR <- function(temp, hum, rain, Rmax){
#   T0 <- 29.0
#   EA <- 0.05
#   N <- 20000
#   kappa <- 8.617e-05; # Boltzmann constant 
#   alpha <- (EFD(T0)*pEA(T0)*MDR(T0, rain, Rmax)*mu_th(T0, hum)^(-1)-mu_th(T0, hum))/(EFD(T0)*pEA(T0)*MDR(T0, rain, Rmax)*mu_th(T0, hum)^(-1))
#   (alpha*N*exp(-EA*((temp-T0)^2)/(kappa*(temp+273.0)*(T0+273.0))))
# }