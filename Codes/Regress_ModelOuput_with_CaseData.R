#---------- compare SEI-SEIR modeled data with AIC case data
rm(list=ls()) #remove previous variable assignments

lr.aic <- read.csv("Concatenated_Data/AIC_Logistic_Regrssion_Data_all_tests.csv", head=T, stringsAsFactors = F)
# lr.aic <- read.csv("Concatenated_Data/AIC_Logistic_Regrssion_NullData.csv", head=T)
# lr.aic.igg.denv <- subset(lr.aic, test == "IgG" & disease == "denv")
# lr.aic.igg.chikv <- subset(lr.aic, test == "IgG" & disease == "chikv")
# lr.aic.pcr.denv <- subset(lr.aic, cohort == "PCR" & disease == 'denv')
# lr.aic.pcr.chikv <- subset(lr.aic, cohort == "PCR" & disease == 'chikv')

#------- Create training and testing datasets
#--- Determine number of observations needed to make a training dataset
# data <- lr.aic.igg.chikv
# smp_size <- floor(0.80 * nrow(data))
# 
# #--- Randomly select observation numbers to ensure sample is randomly selected
# train_ind <- sample(seq_len(nrow(data)), size = smp_size)
# 
# #--- Subset the original data based on the randomly selected observation numbers
# kenya_train <- data[train_ind, ]
# 
# #--- Subset the original data on the remaining observations to make the testing dataset
# kenya_test <- data[-train_ind, ]
# 
# #--- Fit the logistic regression model 
# kenya.test <- glm(result ~ model.Inf.Frac + hospital_site, data=kenya_test, family=binomial(link='logit'))
# 
# #Examine the fit of the logistic regression model
# summary(kenya.test)
# 
# #Perform the Hosmer-Lemeshow Test to determine how well the model fits
# #A significant p-value means the model does not fit well
# library(MKmisc)
# kenya.test.log.pred <- predict(kenya.test, newdata=kenya_test, type="response")
# HL <- HLgof.test(fit = kenya.test.log.pred, obs = kenya_test$result)
# HL$H
# 
# #ROC Curve
# library(pROC)
# library(ROCR)
# pred <- prediction(kenya.test.log.pred, kenya_test$result)
# perf <- performance(pred, measure = "tpr", x.measure = "fpr") #tpr = true positive rate; fpr = false positive rate
# plot(perf)
# 
# acc.perf = performance(pred, measure = "acc")
# plot(acc.perf)
# auc <- performance(pred, measure = "auc")
# auc <- auc@y.values[[1]]
# auc
# 
# tpr <- pred@tp[[1]]/max(pred@tp[[1]])
# fpr <- pred@fp[[1]]/max(pred@fp[[1]])
# fnr <- pred@fn[[1]]/max(pred@fn[[1]])
# tnr <- pred@tn[[1]]/max(pred@tn[[1]])
# mean(tpr)
# mean(fpr)
# mean(fnr)
# mean(tnr)

#----- bootstrap logistic regression
library(MKmisc)
library(pROC)
library(ROCR)

data <- na.omit(lr.aic)

denv.temp.df <- data.frame(H=as.numeric(NA), auc=as.numeric(NA), tpr=as.numeric(NA), fpr=as.numeric(NA), fnr=as.numeric(NA), tnr=as.numeric(NA))
denv.null.df <- data.frame(H=as.numeric(NA), auc=as.numeric(NA), tpr=as.numeric(NA), fpr=as.numeric(NA), fnr=as.numeric(NA), tnr=as.numeric(NA))
chik.temp.df <- data.frame(H=as.numeric(NA), auc=as.numeric(NA), tpr=as.numeric(NA), fpr=as.numeric(NA), fnr=as.numeric(NA), tnr=as.numeric(NA))
chik.null.df <- data.frame(H=as.numeric(NA), auc=as.numeric(NA), tpr=as.numeric(NA), fpr=as.numeric(NA), fnr=as.numeric(NA), tnr=as.numeric(NA))

for (h in 1:4){
  df <- data.frame(H=as.numeric(NA), auc=as.numeric(NA), tpr=as.numeric(NA), fpr=as.numeric(NA), fnr=as.numeric(NA), tnr=as.numeric(NA))
  if (h==1){
    casedata_result <- "visit_a_infected_denv_stfd"
    model_result <- "temp.model.Inf.Frac"
  } else if (h==2){
    casedata_result <- "visit_a_infected_denv_stfd"
    model_result <- "null.model.Inf.Frac"
  } else if (h==3){
    casedata_result <- "visit_a_infected_chikv_stfd"
    model_result <- "temp.model.Inf.Frac"
  } else {
    casedata_result <- "visit_a_infected_chikv_stfd"
    model_result <- "null.model.Inf.Frac"
  }
  for (i in 1:999){
    smp_size <- floor(0.80 * nrow(data))
    train_ind <- sample(seq_len(nrow(data)), size = smp_size)
    kenya_train <- data[train_ind, ]
    kenya_test <- data[-train_ind, ]
    kenya.test <- glm(kenya_test[,casedata_result] ~ kenya_test[,model_result] + kenya_test[,"id_site"], family=binomial(link='logit'))
    kenya.test.log.pred <- predict(kenya.test, newdata=kenya_test, type="response")
    HL <- HLgof.test(fit = kenya.test.log.pred, obs = kenya_test[,casedata_result])
    x <- as.numeric(HL$H[3])
    pred <- prediction(kenya.test.log.pred, kenya_test[,casedata_result])
    perf <- performance(pred, measure = "tpr", x.measure = "fpr") #tpr = true positive rate; fpr = false positive rate
    auc <- performance(pred, measure = "auc")
    auc <- auc@y.values[[1]]
    tpr <- pred@tp[[1]]/max(pred@tp[[1]])
    fpr <- pred@fp[[1]]/max(pred@fp[[1]])
    fnr <- pred@fn[[1]]/max(pred@fn[[1]])
    tnr <- pred@tn[[1]]/max(pred@tn[[1]])
    df[i,] <- c(x, auc, mean(tpr), mean(fpr), mean(fnr), mean(tnr))
    }
  if (h==1){
    denv.temp.df <- df
  } else if (h==2){
    denv.null.df <- df
  } else if (h==3){
    chik.temp.df <- df
  } else {
    chik.null.df <- df
  }
}

sort(unique(chik.null.df$auc))

write.csv(denv.null.df, "Results/bootstrapped_LR_Denv_AIC_Null_Model_v_Cases.csv", row.names = F)

#----- bootstrap logistic regression
# library(MKmisc)
# library(pROC)
# library(ROCR)
# 
# data <- lr.aic
# 
# df <- data.frame(H=as.numeric(NA), auc=as.numeric(NA), tpr=as.numeric(NA), fpr=as.numeric(NA), fnr=as.numeric(NA), tnr=as.numeric(NA))
# 
# for (i in 1:999){
#   smp_size <- floor(0.80 * nrow(data))
#   train_ind <- sample(seq_len(nrow(data)), size = smp_size)
#   kenya_train <- data[train_ind, ]
#   kenya_test <- data[-train_ind, ]
#   kenya.test <- glm(visit_a_infected_denv_stfd ~ model.Inf.Frac + hospital_site, data=kenya_test, family=binomial(link='logit'))
#   kenya.test.log.pred <- predict(kenya.test, newdata=kenya_test, type="response")
#   HL <- HLgof.test(fit = kenya.test.log.pred, obs = kenya_test$result)
#   x <- as.numeric(HL$H[3])
#   pred <- prediction(kenya.test.log.pred, kenya_test$result)
#   perf <- performance(pred, measure = "tpr", x.measure = "fpr") #tpr = true positive rate; fpr = false positive rate
#   auc <- performance(pred, measure = "auc")
#   auc <- auc@y.values[[1]]
#   tpr <- pred@tp[[1]]/max(pred@tp[[1]])
#   fpr <- pred@fp[[1]]/max(pred@fp[[1]])
#   fnr <- pred@fn[[1]]/max(pred@fn[[1]])
#   tnr <- pred@tn[[1]]/max(pred@tn[[1]])
#   df[i,] <- c(x, auc, mean(tpr), mean(fpr), mean(fnr), mean(tnr))
# }
write.csv(df, "Results/bootstrapped_LR_Denv_AIC_IgG_Model_v_Cases.csv", row.names = F)
write.csv(df, "Results/bootstrapped_LR_Chikv_AIC_IgG_Model_v_Cases.csv", row.names = F)
# write.csv(df, "Results/bootstrapped_LR_Denv_AIC_IgG_Null_Model_v_Cases.csv", row.names = F)
# write.csv(df, "Results/bootstrapped_LR_Chikv_AIC_IgG_Null_Model_v_Cases.csv", row.names = F)

df<- read.csv("Results/bootstrapped_LR_Chikv_AIC_IgG_Model_v_Cases.csv", head=T)
# df <- read.csv("Results/bootstrapped_LR_Denv_AIC_IgG_Model_v_Cases.csv", head=T)
df.null <- read.csv("Results/bootstrapped_LR_Chikv_AIC_IgG_Null_Model_v_Cases.csv", head=T)
# df.null <- read.csv("Results/bootstrapped_LR_Denv_AIC_IgG_Null_Model_v_Cases.csv", head=T)

# df.denv <- subset(df, H > 0.05) # remove simulations where the model has a bad fit
df.chikv <- subset(df, H > 0.05) # remove simulations where the model has a bad fit
df.null <- subset(df.null, H > 0.05) 

#----
lr.aic$infected.t1<-as.Date(lr.aic$infected.t1, "%Y-%m-%d")
lr.aic$monyr <- format(lr.aic$infected.t1, "%Y-%m")

test<-ddply(lr.aic, .(monyr),
            summarise,
            totalCases = sum(na.omit(visit_a_infected_denv_stfd)),
            prevalence = totalCases/length(visit_a_infected_denv_stfd))

plot(test$monyr, test$prevalence)
counts <- table(test$totalCases)
barplot(counts, main="Car Distribution", 
        xlab="Number of Gears")
#----
df2 <- subset(denv.temp.df, H > 0.05) # remove simulations where the model has a bad fit
df.null.denv <- subset(denv.null.df, H > 0.05) 
library(ggplot2)
x<-mean(df2$auc)
ggplot(df2, aes(auc,  color=I("blue"), fill=I("blue"))) +
  geom_histogram(alpha = 0.4)+ xlim(0,1) +
  geom_vline(xintercept=x, linetype="dotted")

metrics <- c("tpr", "fpr", "tnr", "fnr", "auc")

for (met in metrics){
  c<- mean(df.denv[,met])
  d<- mean(df.null[,met])
  filepath <- paste0("Figures/modelValidation/LR_Chikv_AIC_IgG_", met, "_Null_v_Temp.tiff")
  tiff(filepath, width=764, height=463)
  abc <- ggplot() + 
    geom_density(data=df.denv, aes(x=df.denv[,met], y=..density..),  col=I("orange"), fill=I("orange"), alpha=I(.3)) +
    geom_density(data=df.null, aes(x=df.null[,met], y=..density..),  col=I("blue"), fill=I("blue"), alpha=I(.1)) +
    xlim(0,1) + labs(x=toupper(met)) +
    geom_vline(xintercept=c(c,d), size=1.5, col=c("orange", "blue"))
  print(abc)
  dev.off()
}


par(mfrow=c(2,3))
hist(df2$tpr, col = "darkblue", xlab = c("True positive rate"), main = "")
hist(df2$fpr, col = "darkblue", xlab = c("False positive rate"), main = "")
hist(df2$auc, col = "darkblue", xlab = c("AUC"), main = "")
hist(df2$tnr, col = "darkblue", xlab = c("True negative rate"), main = "")
hist(df2$fnr, col = "darkblue", xlab = c("False negative rate"), main = "")

#---- mixed model with regression data
r.aic <- read.csv("Concatenated_Data/AIC_Regrssion_Data.csv", head=T)
r.aic$hospital_site[r.aic$hospital_site == 5] <- 4

r.aic.igg.denv <- subset(r.aic, test == "IgG" & disease == "denv")
i <- lm(caseFraction ~ model.Inf.Frac + hospital_site, data=r.aic.igg.denv)
summary(i)
plot(r.aic.igg.denv$caseFraction, r.aic.igg.denv$model.Inf.Frac)

j  <- lm(caseFraction ~ model.Ave.Inf.Frac + hospital_site, data=r.aic.igg.denv)
summary(j)
plot(log(r.aic.igg.denv$caseFraction), log(r.aic.igg.denv$model.Ave.Inf.Frac))

r.aic.igg.chikv <- subset(r.aic, test == "IgG" & disease == "chikv")
k <- lm(caseFraction ~ model.Inf.Frac + hospital_site, data=r.aic.igg.chikv)
summary(k)
plot(log(r.aic.igg.chikv$caseFraction), log(r.aic.igg.chikv$model.Inf.Frac))

l <- lm(caseFraction ~ model.Ave.Inf.Frac + hospital_site, data=r.aic.igg.chikv)
summary(l)
plot(log(r.aic.igg.chikv$caseFraction), log(r.aic.igg.chikv$model.Ave.Inf.Frac))
plot(r.aic.igg.chikv$caseFraction, r.aic.igg.chikv$model.Ave.Inf.Frac)

r.aic.pcr.denv <- subset(r.aic, cohort == "PCR" & disease == 'denv')
r.aic.pcr.chikv <- subset(r.aic, cohort == "PCR" & disease == 'chikv')

denv <- subset(r.aic, disease = "denv")
lm(caseFraction ~ model.Ave.Inf.Frac + hospital_site + test, data=denv)
plot(denv$caseFraction, denv$model.Ave.Inf.Frac)
lm(caseFraction ~ model.Inf.Frac + hospital_site + test, data=denv)
plot(denv$caseFraction, denv$model.Inf.Frac)

#-- hospital site and test as covariates
chikv <- subset(r.aic, disease = "chikv")
lm(caseFraction ~ model.Ave.Inf.Frac + hospital_site + test, data=chikv)
plot(chikv$caseFraction, chikv$model.Ave.Inf.Frac)
lm(caseFraction ~ model.Inf.Frac + hospital_site + test, data=chikv)
plot(chikv$caseFraction, chikv$model.Inf.Frac)
