# polynomial fit to data from Statistical modeling of the effect of rainfall flushing on dengue transmission in Singapore - Plos NTD
# raindat<-data.frame(matrix(ncol=2, nrow=0))
# colnames(raindat) <- c("days_flushed", "OR")
# raindat[1,] <- c(1,1.13)
# raindat[2,] <- c(18,1.16)
# raindat[3,] <- c(36,1.04)
# raindat[4,] <- c(54,0.84)
# raindat[5,] <- c(72,0.62)
# raindat[6,] <- c(90,0.44)
# raindat[7,] <- c(108,0.30)
# raindat[8,] <- c(126,0.20) # forcing model to find lower bound with made up data for rows 8-10
# raindat[9,] <- c(144,0.05)
# raindat[10,] <- c(162,0.00)

library(truncnorm)
rtruncnorm(raindat, a=-Inf, b=Inf, mean = 0, sd = 1)

# https://math.stackexchange.com/questions/573694/bayesian-posterior-with-truncated-normal-prior

raindat<-data.frame(matrix(ncol=2, nrow=0))
colnames(raindat) <- c("days_flushed", "OR")
raindat[1,] <- c(1,1.13)
raindat[2,] <- c(14,1.16)
raindat[3,] <- c(28,1.04)
raindat[4,] <- c(42,0.84)
raindat[5,] <- c(56,0.62)
raindat[6,] <- c(70,0.44)
raindat[7,] <- c(84,0.30)
raindat[8,] <- c(98,0.20) # forcing model to find lower bound with made up data for rows 8-10
raindat[9,] <- c(112,0.05)
raindat[10,] <- c(126,0.00)

plot(raindat$days_flushed, raindat$OR, pch=16)
polyLine <- lm(raindat$OR~poly(raindat$days_flushed, 4, raw=T))
predicted.intervals <- predict(polyLine,data.frame(x=raindat$days_flushed),interval='confidence',level=0.99)
lines(raindat$days_flushed, predicted.intervals[,1],col='blue',lwd=3)
summary(polyLine)


# polynomial fit to data from Schmidt review
hum <- read.csv("Concatenated_Data/Humidity_data.csv", head=T)
plot(hum$kPA, hum$logHazard, pch=16, xlim=c(0,2))
first <- hum[1:19,]
plFirst <- lm(first$logHazard~first$kPA)
predicted.first <- predict(plFirst,data.frame(x=hum$kPA[1:19]),interval='confidence',level=0.99)
lines(hum$kPA[1:19], predicted.first[,1],col='blue',lwd=3)
second <- hum[20:99,]
plSecond <- lm(second$logHazard~second$kPA)
predicted.second <- predict(plSecond,data.frame(x=hum$kPA[20:99]),interval='confidence',level=0.99)
lines(hum$kPA[20:99], predicted.second[,1],col='blue',lwd=3)


polyLine <- lm(hum$logHazard~poly(hum$kPA, 4, raw=T))
predicted.intervals <- predict(polyLine,data.frame(x=hum$kPA),interval='confidence',level=0.99)
lines(hum$kPA, predicted.intervals[,1],col='blue',lwd=3)
summary(polyLine)


# load case data
cases <- read.csv("Concatenated_Data/case_data/merged_case_data.csv", head=T, stringsAsFactors = F)

# load vector data
vectors <- read.csv("Concatenated_Data/vector_data/merged_vector_data.csv", head=T, stringsAsFactors = F)

climateData <- read.csv("Concatenated_Data/climate_data/merged_climate_data.csv", head=T, stringsAsFactors = F)
climateData$Date <- as.Date(climateData$Date, "%Y-%m-%d")

# combine case data, vector data, and modeled data
data <- merge(cases, vectors, by=c("Site", "Date"), all=T)

data$CumRain_wk <- NA
data$CumRain_month <- NA
data$rainydays_wk <- NA
data$rainydays_month <- NA

for (i in 1:nrow(data)){
  climRowIndex <- which(data$Date[i] == climateData$Date)
  data$CumRain_wk[i] <- sum(climateData[((climRowIndex-6):climRowIndex), paste0("GF_", data$Site[i], "_rain")])
  data$CumRain_month[i] <- sum(climateData[((climRowIndex-29):climRowIndex), paste0("GF_", data$Site[i], "_rain")])
  data$rainydays_wk[i] <- sum(climateData[((climRowIndex-6):climRowIndex), paste0("GF_", data$Site[i], "_rain")]>1)
  data$rainydays_month[i] <- sum(climateData[((climRowIndex-29):climRowIndex), paste0("GF_", data$Site[i], "_rain")]>1)
}

data$site2 <- data$Site
data$site2[data$site2 == "Chulaimbo"] <- 1
data$site2[data$site2 == "Kisumu"] <- 2
data$site2[data$site2 == "Msambweni"] <- 3
data$site2[data$site2 == "Ukunda"|data$site2 == "U"] <- 4
data$site2[data$site2 == "Huaquillas"] <- 5
data$site2[data$site2 == "Machala"] <- 6
data$site2[data$site2 == "Portovelo"] <- 7
data$site2[data$site2 == "Zaruma"] <- 8
unique(data$site2)
data$site2 <- as.numeric(data$site2)

data$Country <- ifelse(data$Site=="Chulaimbo"|data$Site=="Kisumu"|data$Site=="Msambweni"|data$Site=="Ukunda", 1, 2)

plot(data$CumRain_wk, data$egg_total_monthly, pch=16, col=data$site2)
test<-subset(data, !is.na(egg_total_weekly))
# test <- subset(data, Country=="Kenya")
# points(test$CumRain, test$aedes_total_weekly, pch=16, main="Kenya")

exponential_decreasing <- function(x){
  exp(0.05)^(-x)
} 

exponential_increasing <- function(x, Tm){
  # if((x > Tm)) # remove if statement if don't want threshold, but need to adjust to not go over 100%
  #   0.001
  # else
    log(x+0.001)/log(Tm+0.001)
} # max needs to be real max, not 55 mm

quadratic <- function(x, c, T0, Tm){
  if((x < T0) | (x > Tm))
    0.001
  else
    (c*(x-T0)*(x-Tm))/abs((c*(Tm/2-T0)*(Tm/2-Tm))) + 0.001
} 

right_skewed <- function(x, c, T0, Tm){
  if((x < T0) | (x > Tm))
    0.001
  else
    (c*(x-T0)*(x-Tm))/(x/4) + 0.001
}

briere <- function(x, c, T0, Tm){
  if((x < T0) | (x > Tm))
    0.001
  else
    c*x*(x-T0)*sqrt(Tm-x)*2 + 0.001
}

ED <- c()
EI <- c()
QD <- c()
RS <- c()
BR <- c()

x <- seq(0,100,1)

for (j in 1:length(x)){ # change 55 to 30 for number of rainy days
  ED<-c(ED, exponential_decreasing(j,55))
  EI<-c(EI, exponential_increasing(j,100))
  QD<-c(QD, quadratic(j,-5.99e-03,1,55))
  RS<-c(RS, right_skewed(j,-5.99e-03,1,55))
  BR<-c(BR, briere(j,7.86e-05,1,55))
}

par(mfrow=c(2,3))

plot(x, RS, type='l', lwd=2, col='blue', xlab='Cumulative rainfall (mm)', ylab='K', main='Right-skewed')
plot(x, QD, type='l', lwd=2, col='blue', xlab='Cumulative rainfall (mm)', ylab='K', main='Quadratic')
plot(x, BR, type='l', lwd=2, col='blue', xlab='Cumulative rainfall (mm)', ylab='K', main='Left-skewed (briere)')
plot(x, ED, type='l', lwd=2, col='blue', xlab='Cumulative rainfall (mm)', ylab='K', main='Exponential decreasing')
plot(x, EI, type='l', lwd=2, col='blue', xlab='Cumulative rainfall (mm)', ylab='K', main='Exponential increasing')


# linear_decreasing  <- function(x, Tm){
#   if((x > Tm))
#     0.0
#   else
#     -(x+0.001)/Tm + 1
# } 
# linear_increasing  <- function(x, Tm){
#   if((x > Tm))
#     0.01
#   else
#     (x+0.001)/Tm
# } 
# convex_decreasing <- function(x, Tm){
#   if((x > Tm))
#     0.0
#   else
#     sqrt(Tm-x)/sqrt(Tm)
# } 
# concave_increasing  <- function(x, Tm){
#   if((x > Tm))
#     0.0
#   else
#     ((x+0.001)/(Tm/x))/Tm
# } 
# LD <- c()
# LI <- c()
# CD <- c()
# CI <- c()
# LD<-c(LD, lin_decreasing(j,55))
# LI<-c(LI, lin_increasing(j,55))
# CD<-c(CD, convex_decreasing(j,55)) 
# CI<-c(CI, concave_increasing(j,55))
# plot(x, LD, type='l', lwd=2, col='blue', xlab='Cumulative rainfall (mm)', ylab='K', main='Linear decreasing')
# plot(x, LI, type='l', lwd=2, col='blue', xlab='Cumulative rainfall (mm)', ylab='K', main='Linear increasing')
# plot(x, CD, type='l', lwd=2, col='blue', xlab='Cumulative rainfall (mm)', ylab='K', main='Convex decreasing')
# plot(x, CI, type='l', lwd=2, col='blue', xlab='Cumulative rainfall (mm)', ylab='K', main='Concave increasing')

# load case data
cases <- read.csv("Concatenated_Data/case_data/merged_case_data.csv", head=T, stringsAsFactors = F)

# load vector data
vectors <- read.csv("Concatenated_Data/vector_data/merged_vector_data.csv", head=T, stringsAsFactors = F)


# load model data
models <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_THR_diff_rain_functions.csv", head=T, stringsAsFactors = F)

models$Mtot <- models$M1 + models$M2 + models$M3
models$Name <- paste(models$Rain_metric, models$R_function, models$Rmax, sep="_")
rNames <- unique(models$Name)
models2 <- unique(models[,c("Site", "Date")]) 

for (i in 1:length(rNames)){
  subdf <- subset(models, Name == rNames[i])
  subdf[paste0("Mtot_", rNames[i])] <- subdf$Mtot
  subdf[paste0("I_", rNames[i])] <- subdf$I
  models2 <- merge(models2, subdf[,c("Site", "Date", paste0("I_", rNames[i]), paste0("Mtot_", rNames[i]))], by=c("Site", "Date"))
}  

# combine case data, vector data, and modeled data
data <- merge(cases, vectors, by=c("Site", "Date"), all=T)
data <- merge(data, models2, by=c("Site", "Date"), all=T)

data$Country <- ifelse(data$Site=="Chulaimbo"|data$Site=="Kisumu"|data$Site=="Msambweni"|data$Site=="Ukunda", "Kenya", "Ecuador")

# function
tau.calc <- function(x, y) {
  t <- cor.test(x, y, method="kendall")
  unname(t$estimate)
}

# set up dataframe
Inames <- names(data)[grep("I_", names(data))]
Mnames <- names(data)[grep("Mtot_", names(data))]
tauNames <- gsub("I_", "tau_", Inames)

corr.rainfall <- data.frame(matrix(ncol=length(tauNames)+2, nrow=0))
colnames(corr.rainfall) <- c("Region", "Yvar", tauNames)#"tau_cum_exp_de", "tau_cum_exp_in", "tau_cum_quadratic", "tau_cum_rskewed", "tau_cum_briere", "tau_days_exp_de", "tau_days_exp_in", "tau_days_qudratic", "tau_days_rskewed", "tau_days_briere")

regionnames <- c("Kenya", "Ecuador")

for (m in 1:length(regionnames)){
  datasub <- subset(data, Country == regionnames[m])
  if (regionnames[m]=="Ecuador"){
    yVars <- c("denv_positive_weekly_any", "denv_positive_0311_weekly", "denv_positive_1418_weekly", "denv_positive_weekly", "chikv_positive_weekly", "aedes_total_weekly")
  } else {
    yVars <- c("denv_positive_weekly", "chikv_positive_weekly", "aedes_total_weekly", "pupae_total_weekly", "late_instar_total_weekly", "early_instar_total_weekly", "egg_total_weekly")
  }
  # sites <- unique(datasub$Site)
  # for (i in 1:length(sites)){
  #   datasub2<-subset(datasub, Site == sites[i])
    for (n in 1:length(yVars)){
      firstLetter <- substr(yVars[n],1,1)
      if (firstLetter == "d"|firstLetter == "c"){
        modVars <- Inames #"I"
      } else {
        modVars <- Mnames #"Mtot"
      }
      tmpvec <- c()
      for (o in 1:length(modVars)){
        # subDF <- datasub2[,c(yVars[n], modVars[o])]
        subDF <- datasub[,c(yVars[n], modVars[o])]
        subDF <- subDF[complete.cases(subDF),]
        if (nrow(subDF)==0){
          tau <- NA  
        } else {
          tau <- tau.calc(subDF[,yVars[n]], subDF[,modVars[o]])
        }
        tmpvec <- c(tmpvec, round(tau, 2))
      }
      tmpvec <- c(regionnames[m], yVars[n], tmpvec) #sites[i]
      corr.rainfall[nrow(corr.rainfall)+1,] <- tmpvec
    }
}
# }

corr.rainfall[3:37] <- lapply(corr.rainfall[3:37], as.numeric)
corr.rainfall$Max_Tau_Name <- colnames(corr.rainfall[3:37])[apply(corr.rainfall[3:37],1,which.max)]

write.csv(corr.rainfall, "Concatenated_Data/model_assessment/tau_diff_rain_functions2.csv", row.names=F)
