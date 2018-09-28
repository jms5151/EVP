# ----- Compare SEI-SEIR modeled output with vector data from traps
rm(list=ls()) #remove previous variable assignments

#--- read in vector data
bg <- read.csv("Concatenated_Data/BG_Regrssion_Data.csv", head=T, as.is=T)
hlc <- read.csv("Concatenated_Data/HLC_Regrssion_Data.csv", head=T, as.is=T)
larvae <- read.csv("Concatenated_Data/Larvae_Regrssion_Data.csv", head=T, as.is=T)
ovitrap <- read.csv("Concatenated_Data/Ovitrap_Regrssion_Data.csv", head=T, as.is=T)
prokopak <- read.csv("Concatenated_Data/Prokopak_Regrssion_Data.csv", head=T, as.is=T)

#--- read in vector data with NULL data - 25.9C
# bg <- read.csv("Concatenated_Data/BG_Regrssion_Data_Null_25.9C.csv", head=T, as.is=T)
# hlc <- read.csv("Concatenated_Data/HLC_Regrssion_Data_Null_25.9C.csv", head=T, as.is=T)
# larvae <- read.csv("Concatenated_Data/Larvae_Regrssion_Data_Null_25.9C.csv", head=T, as.is=T)
# ovitrap <- read.csv("Concatenated_Data/Ovitrap_Regrssion_Data_Null_25.9C.csv", head=T, as.is=T)
# prokopak <- read.csv("Concatenated_Data/Prokopak_Regrssion_Data_Null_25.9C.csv", head=T, as.is=T)

#--- read in vector data with NULL data - 25.9C
# bg <- read.csv("Concatenated_Data/BG_Regrssion_Data_Null_29C.csv", head=T, as.is=T)
# hlc <- read.csv("Concatenated_Data/HLC_Regrssion_Data_Null_29C.csv", head=T, as.is=T)
# larvae <- read.csv("Concatenated_Data/Larvae_Regrssion_Data_Null_29C.csv", head=T, as.is=T)
# ovitrap <- read.csv("Concatenated_Data/Ovitrap_Regrssion_Data_Null_29C.csv", head=T, as.is=T)
# prokopak <- read.csv("Concatenated_Data/Prokopak_Regrssion_Data_Null_29C.csv", head=T, as.is=T)

#----------------------------------------
#------ explore distributions
#----------------------------------------
library(car)
library(MASS)

# plots: HLC total; larvae all; ovitrap eggs; prokopak total
# correlations: BG blood/month; HLC female-total-unfed/weekly

#-- HLC total-female-unfed with negative binomial distribution
#-- larvae early-late-pupae, no distributions are a good fit
#-- ovitrap eggs, lnorm best distribution but not a great fit
#-- prokopak total, no distributions are a good fit
#-- bg bloodfed, no distributions are a good fit

# larvae$instars <- na.omit(larvae$early_instars)+na.omit(larvae$late_instars)
# larvae<-subset(larvae, instars>0)
# larvae$logInstars <-log(larvae$instars)

# df <- ovitrap
# var <- "eggs"
# qqnorm(df[,var])
# qqline(df[,var])
# qqp(df[,var], "lnorm")
# 
# nbinom <- fitdistr(df[,var], "Negative Binomial")
# qqp(df[,var], "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
# mean(df[,var]) # if less than 5 don't use PQL
# 
# poisson <- fitdistr(df[,var], "Poisson")
# qqp(df[,var], "pois", poisson$estimate)
# 
# gamma <- fitdistr(df[,var], "gamma")
# qqp(df[,var], "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

# scale and compare observed v predicted --------------------
bg$scaled_wk_model <- scale(bg$weekly.model.Total)
bg$scaled_adults <- scale(bg$a.aegypti_total)
plot(bg$scaled_wk_model, bg$scaled_adults)
bg$diff <- bg$scaled_wk_model - bg$scaled_adults
cor.test(bg$weekly.model.Total, bg$a.aegypti_total)
x <- lm(bg$a.aegypti_total ~ bg$weekly.model.Total)
summary(x)
fit <- lm(scaled_adults ~ scaled_wk_model + study_site, data=bg)
visreg(fit)
visreg(fit, "study_site", type="contrast")

larvae$scaled_wk_model <- scale(larvae$weekly.model.Total)
larvae$scaled_pupae <- scale(larvae$late_instars)
plot(larvae$scaled_wk_model, larvae$scaled_pupae)
cor.test(larvae$weekly.model.Total, larvae$pupae)
cor.test(larvae$weekly.model.Total, larvae$early_instars)
cor.test(larvae$weekly.model.Total, larvae$late_instars)
x <- lm(larvae$late_instars ~ larvae$weekly.model.Total)
summary(x)
library("visreg")
fit <- lm(late_instars ~ weekly.model.Total + study_site, data=larvae)
visreg(fit)
visreg(fit, "study_site", type="contrast")

plot(ovitrap$weekly.model.Total, ovitrap$eggs)
cor.test(ovitrap$weekly.model.Total, ovitrap$eggs)
x <- lm(ovitrap$eggs ~ ovitrap$weekly.model.Total)
summary(x)

# time series analysis ----
library("gtools")
var_1 <- larvae$pupae
var_2 <- larvae$weekly.model.Total
chg_1 <- diff(var_1)/var_1[-length(var_1)] 
chg_2 <- diff(var_2)/var_2[-length(var_2)] 
running(chg_1, chg_2, fun=cor, width=5, by=1, allow.fewer=TRUE
        , align=c("right"), simplify=TRUE)
running(var_1, var_2, fun=cor, width=5)
ccf(var_1,var_2, main="")
ccf(chg_1,chg_2, main="")

corr = ccf(var_1,var_2)
corr
plot(corr)
plot.ts((larvae$scaled_wk_model-larvae$scaled_pupae))
hist((larvae$scaled_wk_model-larvae$scaled_pupae))
hist((bg$scaled_wk_model-bg$scaled_adults))
hist((ovitrap$weekly.model.Total-ovitrap$eggs))

cor.test(larvae$scaled_wk_model,larvae$scaled_pupae)
barplot(.2, .09, .008)

df <- data.frame(trap=c("Eggs", "Larvae", "Pupae", "Adults"), correlation=c(0.04, 0.18, 0.08, 0.295))
df$trap <- factor(df$trap,levels = c("Eggs", "Larvae", "Pupae", "Adults"))
p<-ggplot(data=df, aes(x=trap, y=correlation)) +
  geom_bar(stat="identity", fill=c("#990033", "#003300", "#006699", "#660099"))
p

# Horizontal bar plot
p + coord_flip()
#----------------------------------------------
# Function that returns Root Mean Squared Error
rmse <- function(error){  
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error){
  mean(abs(error))
}

log10_ceiling <- function(x) {
  nchar(10^(ceiling(log10(x))))
}

library(lme4)
# x<- glm(logInstars ~ weekly.model.Total, data=larvae)
# summary(x)
# GHQ <- glm.nb(a.aegypti_total ~ weekly.model.Total, data = hlc)
# GHQ2 <- glm(a.aegypti_total ~ weekly.model.Total, data = hlc, family=negative.binomial(2))
# summary(GHQ)
# 
# pchisq(deviance(GHQ),df.residual(GHQ),lower.tail=F)
# chisq.test(GHQ)

#------------------------------
#----- bootstrap
#------------------------------

# for (i in 1:nrow(data)){
#   data$zscore[i] <- (data$a.aegypti_female[i]-mean(na.omit(data$a.aegypti_female)))/sd(na.omit(data$a.aegypti_female))
#   data$zscore2[i] <- (data$monthly.model.Total[i]-mean(na.omit(data$monthly.model.Total)))/sd(na.omit(data$monthly.model.Total))
# }
# ovitrap$logEggs <- log(ovitrap$eggs)


#----ovitraps
data <- ovitrap
mosquito.variable<-"eggs"
timespans <- c("weekly.model.Total", "monthly.model.Total")

df <- data.frame(correlation=as.numeric(NA), mean_fit=as.numeric(NA), sd_fit=as.numeric(NA), 
                 RootMeanSqErr=as.numeric(NA), MeanAbsErr=as.numeric(NA), sameMagnitude=as.numeric(NA),
                 oneMagnitudeDiff=as.numeric(NA), twoMagnitudeDiff=as.numeric(NA),
                 threeMagnitudeDiff=as.numeric(NA), timespan=as.character(NA), stringsAsFactors=FALSE)

df2 <- data.frame(correlation=as.numeric(NA), mean_fit=as.numeric(NA), sd_fit=as.numeric(NA), 
                  RootMeanSqErr=as.numeric(NA), MeanAbsErr=as.numeric(NA), sameMagnitude=as.numeric(NA),
                  oneMagnitudeDiff=as.numeric(NA), twoMagnitudeDiff=as.numeric(NA),
                  threeMagnitudeDiff=as.numeric(NA), timespan=as.character(NA), stringsAsFactors=FALSE)

for (time.variable in timespans){
    for (i in 1:999){
      smp_size <- floor(0.80 * nrow(data))
      train_ind <- sample(seq_len(nrow(data)), size = smp_size)
      kenya_train <- data[train_ind, ]
      kenya_test <- data[-train_ind, ]
      kenya.test <- lm(kenya_test[,mosquito.variable] ~ kenya_test[,time.variable])# + kenya_test[,"study_site"])
      kenya_test$predictedY <- predict(kenya.test, newdata=kenya_test, type="response")
      fit <- (kenya_test$predictedY - kenya_test[,mosquito.variable])/kenya_test[,mosquito.variable] #(predicted-observed)/observed
      cor <- cor.test(kenya_test$predictedY, kenya_test[,mosquito.variable])
      rmse_val <-rmse(is.finite(fit))
      mae_val <- mae(is.finite(fit))
      orderMagnitudeDiff <- abs(log10_ceiling(kenya_test$predictedY) - log10_ceiling(kenya_test[,mosquito.variable]))
      sameMag <- sum(orderMagnitudeDiff == 0)/length(orderMagnitudeDiff)
      oneMagDiff  <- sum(orderMagnitudeDiff <= 1)/length(orderMagnitudeDiff)
      twoMagDiff <- sum(orderMagnitudeDiff <= 2)/length(orderMagnitudeDiff)
      threeMagDiff <- sum(orderMagnitudeDiff <= 3)/length(orderMagnitudeDiff)      
      df[i,] <- c(cor$estimate, mean(is.finite(fit)), sd(is.finite(fit)), rmse_val, mae_val, sameMag, oneMagDiff, twoMagDiff, threeMagDiff, time.variable)
    }
  df2 <- rbind(df2, df)
}  

df2$mosquito_metric <- "Eggs"
df2$trap <- "Ovitrap"
ovitrapResults <- df2

# write.csv(df2, "Results/bootstrapped_Regression_Model_v_oviraps.csv", row.names = F)

#--- larvae
larvae$instars <- na.omit(larvae$early_instars) + na.omit(larvae$late_instars)
data <- larvae

# mosquito.variable<-"instars"
m.metrics <- c("instars", "pupae")
timespans <- c("weekly.model.Total", "monthly.model.Total")

df <- data.frame(correlation=as.numeric(NA), mean_fit=as.numeric(NA), sd_fit=as.numeric(NA), 
                 RootMeanSqErr=as.numeric(NA), MeanAbsErr=as.numeric(NA), sameMagnitude=as.numeric(NA),
                 oneMagnitudeDiff=as.numeric(NA), twoMagnitudeDiff=as.numeric(NA),
                 threeMagnitudeDiff=as.numeric(NA), timespan=as.character(NA), 
                 mosquito_metric=as.character(NA), stringsAsFactors=FALSE)

df2 <- data.frame(correlation=as.numeric(NA), mean_fit=as.numeric(NA), sd_fit=as.numeric(NA), 
                  RootMeanSqErr=as.numeric(NA), MeanAbsErr=as.numeric(NA), sameMagnitude=as.numeric(NA),
                  oneMagnitudeDiff=as.numeric(NA), twoMagnitudeDiff=as.numeric(NA),
                  threeMagnitudeDiff=as.numeric(NA), timespan=as.character(NA), 
                  mosquito_metric=as.character(NA), stringsAsFactors=FALSE)

for (mosquito.variable in m.metrics){
  for (time.variable in timespans){
    for (i in 1:999){
      smp_size <- floor(0.80 * nrow(data))
      train_ind <- sample(seq_len(nrow(data)), size = smp_size)
      kenya_train <- data[train_ind, ]
      kenya_test <- data[-train_ind, ]
      kenya.test <- glm(kenya_test[,mosquito.variable] ~ kenya_test[,time.variable])# + kenya_test[,"study_site"])
      kenya_test$predictedY <- predict(kenya.test, newdata=kenya_test, type="response")
      fit <- (kenya_test$predictedY - kenya_test[,mosquito.variable])/kenya_test[,mosquito.variable] #(predicted-observed)/observed
      cor <- cor.test(kenya_test$predictedY, kenya_test[,mosquito.variable])
      rmse_val <-rmse(is.finite(fit))
      mae_val <- mae(is.finite(fit))
      orderMagnitudeDiff <- abs(log10_ceiling(kenya_test$predictedY) - log10_ceiling(kenya_test[,mosquito.variable]))
      sameMag <- sum(orderMagnitudeDiff == 0)/length(orderMagnitudeDiff)
      oneMagDiff  <- sum(orderMagnitudeDiff <= 1)/length(orderMagnitudeDiff)
      twoMagDiff <- sum(orderMagnitudeDiff <= 2)/length(orderMagnitudeDiff)
      threeMagDiff <- sum(orderMagnitudeDiff <= 3)/length(orderMagnitudeDiff)      
      df[i,] <- c(cor$estimate, mean(is.finite(fit)), sd(is.finite(fit)), rmse_val, mae_val, sameMag, oneMagDiff, twoMagDiff, threeMagDiff, time.variable, mosquito.variable)
    }
    df2 <- rbind(df2, df)
  }
}

df2$trap <- "Larvae"
larvaeResults <- df2

# write.csv(df2, "Results/bootstrapped_Regression_Model_v_oviraps.csv", row.names = F)
#plot(kenya_test$predictedY, kenya_test$a.aegypti_female)
# hist(df$correlation)
# hist(df$mean_fit)
# hist(df$sd_fit)

#-------- bg, prokopak, hlc
# m.metrics <- c("a.aegypti_male", "a.aegypti_unfed", "a.aegypti_bloodfed", "a.aegypti_halfgravid",
#                "a.aegypti_gravid", "a.aegypti_gravid", "a.aegypti_female", "a.aegypti_total")

m.metrics <- c("a.aegypti_total")

timespans <- c("weekly.model.Total", "monthly.model.Total")

df <- data.frame(correlation=as.numeric(NA), mean_fit=as.numeric(NA), sd_fit=as.numeric(NA), 
                 RootMeanSqErr=as.numeric(NA), MeanAbsErr=as.numeric(NA), sameMagnitude=as.numeric(NA),
                 oneMagnitudeDiff=as.numeric(NA), twoMagnitudeDiff=as.numeric(NA),
                 threeMagnitudeDiff=as.numeric(NA), timespan=as.character(NA), 
                 mosquito_metric=as.character(NA), trap=as.character(NA), stringsAsFactors=FALSE)

df2 <- data.frame(correlation=as.numeric(NA), mean_fit=as.numeric(NA), sd_fit=as.numeric(NA), 
                  RootMeanSqErr=as.numeric(NA), MeanAbsErr=as.numeric(NA), sameMagnitude=as.numeric(NA),
                  oneMagnitudeDiff=as.numeric(NA), twoMagnitudeDiff=as.numeric(NA),
                  threeMagnitudeDiff=as.numeric(NA), timespan=as.character(NA), 
                  mosquito_metric=as.character(NA), trap=as.character(NA), stringsAsFactors=FALSE)

for (i in 1:3){
  if (i == 1){
    data <- hlc
    trap <- "hlc"
  } else if (i == 2){
    data <- bg
    trap <- "bg"
  } else {
    data <- prokopak
    trap <- "prokopak"
  }
  for (mosquito.variable in m.metrics){
    for (time.variable in timespans){
      for (i in 1:999){
        smp_size <- floor(0.80 * nrow(data))
        train_ind <- sample(seq_len(nrow(data)), size = smp_size)
        kenya_train <- data[train_ind, ]
        kenya_test <- data[-train_ind, ]
        kenya.test <- glm(kenya_test[,mosquito.variable] ~ kenya_test[,time.variable])# + kenya_test[,"study_site"])
        kenya_test$predictedY <- predict(kenya.test, newdata=kenya_test, type="response")
        fit <- (kenya_test$predictedY - kenya_test[,mosquito.variable])/kenya_test[,mosquito.variable] #(predicted-observed)/observed
        cor <- cor.test(kenya_test$predictedY, kenya_test[,mosquito.variable])
        rmse_val <-rmse(is.finite(fit))
        mae_val <- mae(is.finite(fit))
        orderMagnitudeDiff <- abs(log10_ceiling(kenya_test$predictedY) - log10_ceiling(kenya_test[,mosquito.variable]))
        sameMag <- sum(orderMagnitudeDiff == 0)/length(orderMagnitudeDiff)
        oneMagDiff  <- sum(orderMagnitudeDiff <= 1)/length(orderMagnitudeDiff)
        twoMagDiff <- sum(orderMagnitudeDiff <= 2)/length(orderMagnitudeDiff)
        threeMagDiff <- sum(orderMagnitudeDiff <= 3)/length(orderMagnitudeDiff)      
        df[i,] <- c(cor$estimate, mean(is.finite(fit)), sd(is.finite(fit)), rmse_val, mae_val, sameMag, oneMagDiff, twoMagDiff, threeMagDiff, time.variable, mosquito.variable, trap)
      }
      df2 <- rbind(df2, df)
    }  
  }
}

resultsOtherTraps<-df2

#---- concatenate datasets
vectorData <-rbind(larvaeResults, ovitrapResults, resultsOtherTraps)
vectorData2 <- na.omit(vectorData)

write.csv(vectorData2, "Results/bootstrapped_Regression_Model_v_Traps.csv", row.names = F)
# write.csv(vectorData2, "Results/bootstrapped_Regression_Model_v_Traps_Null_25.9C.csv", row.names = F)
# write.csv(vectorData2, "Results/bootstrapped_Regression_Model_v_Traps_Null_29C.csv", row.names = F)


# for (i in 1:3){
#   if (i == 1){
#     data <- hlc
#     trap <- "hlc"
#   } else if (i == 2){
#     data <- bg
#     trap <- "bg"
#   } else {
#     data <- prokopak
#     trap <- "prokopak"
#   }
#   for (mosquito.variable in m.metrics){
#     for (time.variable in timespans){
#       for (i in 1:999){
#         smp_size <- floor(0.80 * nrow(data))
#         train_ind <- sample(seq_len(nrow(data)), size = smp_size)
#         kenya_train <- data[train_ind, ]
#         kenya_test <- data[-train_ind, ]
#         kenya.test <- glm(kenya_test[,mosquito.variable] ~ kenya_test[,time.variable] + kenya_test[,"study_site"])
#         kenya_test$predictedY <- predict(kenya.test, newdata=kenya_test, type="response")
#         fit <- kenya_test$predictedY - kenya_test$a.aegypti_total
#         r2 <- summary(kenya.test)$r.squared
#         df[i,] <- c(r2, mean(fit), sd(fit), time.variable, mosquito.variable, trap)
#       }
#      df2 <- rbind(df2, df)
#     }  
#   }
# }
# 
# write.csv(df2, "Results/bootstrapped_Regression_Model_v_Traps.csv", row.names = F)

#ROC Curve
library(pROC)
library(ROCR)
pred <- prediction(kenya.test.log.pred, kenya_test$a.aegypti_total)
perf <- performance(pred, measure = "tpr", x.measure = "fpr") #tpr = true positive rate; fpr = false positive rate
plot(perf)

acc.perf = performance(pred, measure = "acc")
plot(acc.perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

tpr <- pred@tp[[1]]/max(pred@tp[[1]])
fpr <- pred@fp[[1]]/max(pred@fp[[1]])
fnr <- pred@fn[[1]]/max(pred@fn[[1]])
tnr <- pred@tn[[1]]/max(pred@tn[[1]])
mean(tpr)
mean(fpr)
mean(fnr)
mean(tnr)

