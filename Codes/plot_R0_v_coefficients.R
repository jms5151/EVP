rm(list=ls()) #remove previous variable assignments

# R0 plot
Aedes.R0.out <- read.csv("Concatenated_Data/AedesR0Out.csv", header = T)

library(shape)
priorLit <- read.csv("Concatenated_Data/Temperature_v_coefficients.csv", head=T)
priorLit$Tmean2 <- (priorLit$Tmax+priorLit$Tmin)/2
priorLit$temperature <- ifelse(is.na(priorLit$Tmean), priorLit$Tmean2, priorLit$Tmean)
priorLit$temperature <- round(priorLit$temperature,1)

priorLit2 <- merge(priorLit, Aedes.R0.out, by="temperature", all.x=T)
priorLit2$Coefficient2 <- ifelse(priorLit2$Coefficient>3, priorLit2$Coefficient/3, priorLit2$Coefficient)
priorLit2$Coefficient2 <- priorLit2$Coefficient2/4
priorLit2$y1 <- priorLit2$aegypti.R0.median + priorLit2$Coefficient2
priorLit2$y1 <- ifelse(priorLit2$y1< -0.1, -0.1, priorLit2$y1)

# plot
par(mar = c(4, 5, 4, 4))
plot(aegypti.R0.median ~ temperature, xlim = c(12,38), ylim=c(-.1,1),lwd = 2, lty = 1, 
     col = 1, type = "l", xlab = expression(paste("Temperature (",degree,"C)")),
     ylab = expression(paste("Relative R"[0])), main = "", data = Aedes.R0.out, cex.axis = 1.2, cex.lab = 1.2)

par(new = TRUE)
points(priorLit2$temperature, priorLit2$aegypti.R0.median, pch=16)

x<-priorLit2$aegypti.R0.median-priorLit2$Coefficient2
Arrows(priorLit2$temperature, priorLit2$aegypti.R0.median, priorLit2$temperature, priorLit2$y1, code = 2, arr.length = 0.2, arr.type="triangle")
Arrows(x0 = 12, y0 = 0.9, x1 = 12, y1 = 0.95, length = 0.25, code = 2, arr.length = 0.2, arr.type="triangle")
Arrows(x0 = 12, y0 = 0.85, x1 = 12, y1 = 0.80, length = 0.25, code = 2, arr.length = 0.2, arr.type="triangle")
text(15, 0.93, "Positive coefficient")
text(15, 0.82, "Negative coefficient")

# # plot with slopes
# par(mfrow = c(1,1), las = 1)
# plot(aegypti.R0.median ~ temperature, xlim = c(5,45), ylim=c(-.1,1),lwd = 2, lty = 1, 
#      col = 1, type = "l", xlab = expression(paste("Temperature (",degree,"C)")),
#      ylab = expression(paste("Relative R"[0])), main = "", data = Aedes.R0.out, cex.axis = 1.2, cex.lab = 1.2)
# 
# priorLit <- read.csv("Concatenated_Data/Temperature_v_coefficients.csv", head=T)
# priorLit$y0 <- priorLit$Coefficient*priorLit$Tmin
# priorLit$y1 <- priorLit$Coefficient*priorLit$Tmax
# 
# # separate positive and negative coefficient data for scaling purposes
# priorLitPos <- subset(priorLit, Coefficient > 0)
# priorLitPos$y0 <- priorLitPos$y0/max(priorLitPos$y1)
# priorLitPos$y1 <- priorLitPos$y1/max(priorLitPos$y1)
# 
# for (i in 1:nrow(priorLitPos)){
#   segments(x0=priorLitPos$Tmin[i], y0=priorLitPos$y0[i],
#            x1=priorLitPos$Tmax[i], y1=priorLitPos$y1[i],
#            col='red', lwd=2)
# }
# 
# priorLitNeg <- subset(priorLit, Coefficient < 0)
# priorLitNeg$y0 <- priorLitNeg$y0/min(priorLitNeg$y1)
# priorLitNeg$y1 <- priorLitNeg$y1/min(priorLitNeg$y1)
# 
# for (i in 1:nrow(priorLitNeg)){
#   segments(x0=priorLitNeg$Tmin[i], y0=priorLitNeg$y1[i],
#            x1=priorLitNeg$Tmax[i], y1=priorLitNeg$y0[i],
#            col='blue', lwd=2)
# }


# range01 <- function(x){(x-min(x))/(max(x)-min(x))}

# # priorLitNeg$Coefficent2 <- range11(priorLitNeg$Coefficent)
# priorLitNeg$diff <- priorLitNeg$y0-priorLitNeg$y1
# priorLitNeg$y0 <- priorLitNeg$y0-min(priorLitNeg$y0)
# priorLitNeg$y1 <- priorLitNeg$y1-min(priorLitNeg$y1)
# # priorLitNeg$y0 <- range01(priorLitNeg$y0)
# # priorLitNeg$y1 <- range01(priorLitNeg$y1)
# # priorLit$y0 <- range11(priorLit$y0)
# # priorLit$y1 <- range11(priorLit$y1)
# 
# 
# # plot(1,xlim=c(0,40), ylim=c(-0,22))
# 
# 
# 
# lines(aegypti.R0.median*20 ~ temperature, xlim = c(min(priorLit$Tmin),max(priorLit$Tmax)), lwd = 2, lty = 1, 
#      col = 1, type = "l", xlab = expression(paste("Temperature (",degree,"C)")),
#      ylab = expression(paste("Relative R"[0])), main = "", data = Aedes.R0.out, cex.axis = 1.2, cex.lab = 1.2)
# 
# x1 <- stats::runif(5)
# x2 <- stats::runif(5)+2
# y <- stats::rnorm(10)
# 
# 
# plot(c(x1,x2), y)
# 
# 
# segments(x1, y[1:5], x2, y[6:10], col= 'blue')
# 
# plot(c(priorLit$Tmin,priorLit$Tmax), priorLit$Coefficent)
# segments(x1, y[1:5], x2, y[6:10], col= 'blue')
