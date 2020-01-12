# plot rainfall and humidity (SVPD) functions ----------------------------------------------
rm(list=ls()) #remove previous variable assignments

source("C:/Users/Jeremy/Box Sync/DENV/Codes/SEI-SEIR_model_THR.R")

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
title("a. Brière", adj = 0.05, line = -0.9)
par(cex=1.2, mai = c(1, 0.4, 0.1, 0.3))
plot(x, QD, type='l', lwd=2, col='blue', ylab="", xlab="", ylim=c(0,160000), yaxt='n')
Axis(side=2, labels=FALSE)
title("b. Quadratic", adj = 0.05, line = -0.9)
par(cex=1.2, mai = c(1, 0.1, 0.1, 0.5))
plot(x, IN, type='l', lwd=2, col='blue', ylab="", xlab="", ylim=c(0,160000), yaxt='n')
Axis(side=2, labels=FALSE)
title("c. Inverse", adj = 0.05, line = -0.9)
mtext('14-day cumulative rainfall (mm)', side=3, line=-25, outer=TRUE, cex=1.2)

# plot humidity funciton -------------------------------------------------------
x<-c()
h<-seq(0.2,3,0.2)

for(i in 1:length(h)){
  x<-c(x,mu_th(29,h[i],1/12))
}

par(mar = c(5.1, 5.1, 4.1, 1))
plot(h,x, type='l', col='orange', lwd=2, cex=1.2, xlab="", ylab="", yaxt='n')
axis(side=2, las=2)
title(ylab='Mortality rate', line=4)
title(xlab='Saturation vapor pressure deficit (kilopascals)', line=2.5)
