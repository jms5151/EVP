# case data t-tests -----------------------
rm(list=ls()) #remove previous variable assignments
rm(list= ls()[!(ls() %in% c('R01_lab_results','redcap_data'))])

# library(ggplot2)
library(dplyr)

aic_data <- read.csv("Concatenated_Data/AIC_Logistic_Regrssion_Data_all_tests.csv", head=T, stringsAsFactors = F)

# Wilcoxin rank sum test to compare equality of means
wilcox.test(temp.model.Inf ~ visit_a_infected_denv_stfd, data=aic_data)
wilcox.test(temp.model.Inf ~ visit_a_infected_chikv_stfd, data=aic_data)

# plot distributions

aic_data2 <- subset(aic_data, !is.na(visit_a_infected_denv_stfd) & !is.na(infected.t0))
aic_data2$denv <- as.character(aic_data2$visit_a_infected_denv_stfd)
ggplot(aic_data2, aes(x=temp.model.Inf, fill=denv)) + geom_density(alpha=.3)
boxplot(temp.model.Inf ~ denv, data=aic_data2)

aic_data3 <- subset(aic_data, !is.na(visit_a_infected_chikv_stfd) & !is.na(infected.t0))
aic_data3$chikv <- as.character(aic_data3$visit_a_infected_chikv_stfd)
ggplot(aic_data3, aes(x=temp.model.Inf, fill=chikv)) + geom_density(alpha=.3)
boxplot(temp.model.Inf ~ chikv, data=aic_data3)

chik_neg <- subset(aic_data, visit_a_infected_chikv_stfd == 0)
chik_pos <- subset(aic_data, visit_a_infected_chikv_stfd == 1)
denv_neg <- subset(aic_data, visit_a_infected_denv_stfd == 0)
denv_pos <- subset(aic_data, visit_a_infected_denv_stfd == 1)

# Wilcoxin rank sum test with randomly sampled zeros for equal classes
chik.neg.df <- sample_n(chik_neg, nrow(chik_pos))
chik.df <- rbind(chik_pos, chik.neg.df)
wilcox.test(temp.model.Inf.Frac ~ visit_a_infected_chikv_stfd, data=chik.df)

denv.neg.df <- sample_n(denv_neg, nrow(denv_pos))
denv.df <- rbind(denv_pos, denv.neg.df)
wilcox.test(temp.model.Inf.Frac ~ visit_a_infected_denv_stfd, data=denv.df)
