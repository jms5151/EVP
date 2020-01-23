# sign test --------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
# library(dplyr)
library(tidyverse)
library(car)

# load data
load("Concatenated_Data/model_v_data_cases.RData")
load("Concatenated_Data/model_v_data_aedes.RData")

# vectors: sign test --------------------------------------
# subset vector data
vectors <- vectors_and_mods[,c("Site", "Date", "Mtot", "aedes_total")]
vectors <- vectors[complete.cases(vectors),]

# calculate whether mosquitoes were predicted and observed to increase or decrease
vectors <- mutate(vectors, Mtot_diff = Mtot - lag(Mtot), aedes_diff = aedes_total - lag(aedes_total))
vectors$sign <- ifelse((vectors$Mtot_diff < 0 & vectors$aedes_diff < 0)|(vectors$Mtot_diff > 0 & vectors$aedes_diff > 0), 1, 0)

# determine first date for each site and make sign NA for that time point
earliestDates <- vectors %>% group_by(Site) %>% summarize(minDate = min(Date))
for (i in 1:nrow(earliestDates)){
  rowX <- which(vectors$Site == earliestDates$Site[i] & vectors$Date == earliestDates$minDate[i])
  vectors$sign[rowX] <- NA
}

# sign test
binom.test(sum(vectors$sign==1, na.rm=T), sum(!is.na(vectors$sign)), p = 0.5, alternative = c("two.sided"), conf.level = 0.95)

# cases: sign test --------------------------------------
# subset case data
cases <- cases_and_mods[,c("Site", "Date", "I", "denv_positive")]
cases <- cases[complete.cases(cases),]

# calculate whether cases were predicted and observed to increase or decrease
cases <- mutate(cases, I_diff = I - lag(I), cases_diff = denv_positive - lag(denv_positive))
cases$sign <- ifelse((cases$I_diff < 0 & cases$cases_diff < 0)|(cases$I_diff > 0 & cases$cases_diff > 0), 1, 0)

# determine first date for each site and make sign NA for that time point
earliestDates <- cases %>% group_by(Site) %>% summarize(minDate = min(Date))
for (i in 1:nrow(earliestDates)){
  rowX <- which(cases$Site == earliestDates$Site[i] & cases$Date == earliestDates$minDate[i])
  cases$sign[rowX] <- NA
}

# sign test
binom.test(sum(cases$sign==1, na.rm=T), sum(!is.na(cases$sign)), p = 0.5, alternative = c("two.sided"), conf.level = 0.95)

# vectors: anova ----------------------------------------
vectors$Year <- format(vectors$Date, "%Y")
vectors2 <- vectors %>% 
  group_by(Site, Year) %>% 
  summarize(percAnnualPred = sum(aedes_total, na.rm=T)/sum(Mtot)) #%>%

# visualzie
boxplot(vectors2$percAnnualPred~vectors2$Site)

# check assumption of homogeneity of variance
leveneTest(vectors2$percAnnualPred~vectors2$Site)

# anova
anovaVectors <- aov(vectors2$percAnnualPred~vectors2$Site)
summary(anovaVectors)

# check residuals
resVectors <- anovaVectors$residuals
hist(resVectors)

# Tukey's posthoc test to find out which sites differ
TukeyHSD(anovaVectors)

# cases: anova ----------------------------------------
cases$Year <- format(cases$Date, "%Y")
# cases$I <- ifelse(cases$Site == "Zaruma" & cases$I < 3, 3, cases$I)
cases2 <- cases %>% 
  group_by(Site, Year) %>% 
  summarize(percAnnualPred = sum(denv_positive, na.rm=T)/sum(I)) %>%
  filter(percAnnualPred < 1)

# visualzie
boxplot(cases2$percAnnualPred~cases2$Site)

# check assumption of homogeneity of variance
leveneTest(cases2$percAnnualPred~cases2$Site)

# anova
anovaCases <- aov(cases2$percAnnualPred~cases2$Site)
summary(anovaCases)

# check residuals
resCases <- anovaCases$residuals
hist(resCases)

# Tukey's posthoc test to find out which sites differ
TukeyHSD(anovaCases)
