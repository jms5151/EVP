# plot rainfall and humidity functions --------------------------------------------
rm(list=ls()) #remove previous variable assignments

# rainfall ------------------------------------------------------------------------
source("C:/Users/Jamie/Box Sync/SEI-SEIR_Arboviruses/SEI-SEIR_model_TRH.R")

BR <- c()
QD <- c()
IN <- c()

x <- seq(0, 130, 1)

for (i in 1:length(x)){
  BR <- c(BR, K_thr_briere(29, x[i], 123, 166000))
  QD <- c(QD, K_thr_quadratic(29, x[i], 123, 166000))
  IN <- c(IN, K_thr_inverse(29, x[i], 123, 166000))
}

par(mfrow=c(1, 3), cex=1.2, mai = c(1, 1, 0.1, 0))
plot(x, BR, type='l', lwd=2, col='blue', ylab=c("Carrying capacity"), xlab="", ylim=c(0,160000))
title("A. Brière", adj = 0.05, line = -0.9)
par(cex=1.2, mai = c(1, 0.4, 0.1, 0.3))
plot(x, QD, type='l', lwd=2, col='blue', ylab="", xlab="", ylim=c(0,160000), yaxt='n')
Axis(side=2, labels=FALSE)
title("B. Quadratic", adj = 0.05, line = -0.9)
par(cex=1.2, mai = c(1, 0.1, 0.1, 0.5))
plot(x, IN, type='l', lwd=2, col='blue', ylab="", xlab="", ylim=c(0,160000), yaxt='n')
Axis(side=2, labels=FALSE)
title("C. Inverse", adj = 0.05, line = -0.9)
mtext('14-day cumulative rainfall (mm)', side=3, line=-25, outer=TRUE, cex=1.2)