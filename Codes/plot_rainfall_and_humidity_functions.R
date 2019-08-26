# plot rainfall and humidity functions --------------------------------------------
rm(list=ls()) #remove previous variable assignments

# rainfall ------------------------------------------------------------------------
source("Codes/SEI-SEIR_model_TR.R")

BR <- c()
QD <- c()
# RS <- c()
# PL <- c()
IN <- c()

x <- seq(0, 130, 1)

for (i in 1:length(x)){
  BR <- c(BR, K_tr_briere(29, x[i], 123, 20000))
  QD <- c(QD, K_tr_quadratic(29, x[i], 123, 20000))
  # RS <- c(RS, K_tr_right_skewed(29, x[i], 123, 20000))
  # PL <- c(PL, K_tr_poly(29, x[i], 123, 20000))
  IN <- c(IN, K_tr_inverse(29, x[i], 123, 20000))
}

par(mfrow=c(1, 3), cex=1.2, mai = c(1, 1, 0.1, 0.1))
plot(x, BR, type='l', lwd=2, col='blue', ylab=c("Carrying capacity"), xlab=c("Cumulative\n rainfall (mm)"), ylim=c(0,25000))
title("A. Brière", adj = 0.05, line = -0.9)
plot(x, QD, type='l', lwd=2, col='blue', ylab=c("Carrying capacity"), xlab=c("Cumulative\n rainfall (mm)"), ylim=c(0,205000))
title("B. Quadratic", adj = 0.05, line = -0.9)
# plot(x, RS, type='l', lwd=2, col='blue', ylab=c("Carrying capacity"), xlab=c("Monthly accumulated\n rainfall (mm)"), ylim=c(0,205000))
# title("C. Right-skewed", adj = 0.05, line = -0.9)
plot(x, IN, type='l', lwd=2, col='blue', ylab=c("Carrying capacity"), xlab=c("Cumulative\n rainfall (mm)"), ylim=c(0,25000))
title("C. Inverse", adj = 0.05, line = -0.9)
# plot(x, PL, type='l', lwd=2, col='blue', ylab=c("Carrying capacity"), xlab=c("Cumulative\n rainfall (mm)"), ylim=c(0,25000))
# title("C. Polynomial", adj = 0.05, line = -0.9)

# humidity ------------------------------------------------------------------------
source("Codes/SEI-SEIR_model_TRH.R")
mu_th_linear <- function(temp, hum){
  inverted_quadratic(temp,-1.48e-01, 9.16, 37.73)+(1-(0.643*(hum/100)))*0.05
}

mu_th_sigmoidal <- function(temp, hum){
  inverted_quadratic(temp,-1.48e-01, 9.16, 37.73)+(1.01-(0.7/(1+exp(-hum + 40))))*0.05
}

mu_th <- function(temp, hum){
  if (hum <= 1){
    inverted_quadratic(temp,-1.48e-01, 9.16, 37.73)+(1-(0.01256 + 2.00893*hum))*0.01
  } else {
    inverted_quadratic(temp,-1.48e-01, 9.16, 37.73)+(1-(1.2248 + 0.2673*hum))*0.01
  }
}

# LN <- c()
# SG <- c()
th <- c()

# x <- seq(0, 100, 1)
x <- seq(0, 6, 0.05)

for (i in 1:length(x)){
  # LN <- c(LN, mu_th_linear(29, x[i]))
  # SG <- c(SG, mu_th_sigmoidal(29, x[i]))
  th <- c(th, mu_th(29, x[i]))
}

plot(x, th, type='l', lwd=2, col='orange', ylab=c("Daily mortality rate"), xlab=c("Relative humidity"))


par(mfrow=c(1, 2), cex=1.2, mai = c(1, 1, 0.1, 0.1))
plot(x, LN, type='l', lwd=2, col='orange', ylab=c("Daily mortality rate"), xlab=c("Relative humidity"), ylim=c(0.055, 0.091))
title("A. Linear", adj = 0.05, line = -0.9)
plot(x, SG, type='l', lwd=2, col='orange', ylab=c("Daily mortality rate"), xlab=c("Relative humidity"), ylim=c(0.055, 0.091))
title("B. Sigmoidal", adj = 0.05, line = -0.9)
