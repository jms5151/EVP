# panel analysis exploration -----------------------------------
# https://www.princeton.edu/~otorres/Panel101R.pdf

rm(list=ls()) #remove previous variable assignments

# load libraries 
library(foreign)

# adults.site <- read.csv("Kenya/Concatenated_Data/vector/K_suitability_adults_site_lags.csv", head=T, stringsAsFactors = T)
adults.house <- read.csv("Kenya/Concatenated_Data/vector/K_suitability_adults_house_lags.csv", head=T, stringsAsFactors = T)
# larvae.site <- read.csv("Kenya/Concatenated_Data/vector/K_suitability_larvae_site_lags.csv", head=T, stringsAsFactors = T)
larvae.house <- read.csv("Kenya/Concatenated_Data/vector/K_suitability_larvae_house_lags.csv", head=T, stringsAsFactors = T)
eggs.site <- read.csv("Kenya/Concatenated_Data/vector/K_suitability_eggs_site_lags.csv", head=T, stringsAsFactors = T)
eggs.house <- read.csv("Kenya/Concatenated_Data/vector/K_suitability_eggs_house_lags.csv", head=T, stringsAsFactors = T)

# coplots
coplot(egg_total ~ mn.yr|vector_house_id, type="l", data=eggs.house) # Lines
coplot(egg_total ~ mn.yr|study_site, type="b", data=eggs.site) # Points and lines

library(car)
scatterplot(egg_total ~ mn.yr|study_site, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=eggs.site)

library(gplots)
plotmeans(egg_total ~ study_site, main="Heterogeineity across sites", data=eggs.house)

library(plm)
fixed <- plm(egg_total ~ T_suitability + R_suitability_hump + H_suitability, data=eggs.house, index=c("house", "mn.yr"), model="within")#, effect="twoways" is only available for balanced panels
summary(fixed)
# fixef(fixed)

fixed <- plm(mean_eggs ~ T_suitability + R_suitability_hump + H_suitability, data=eggs.site, index=c("study_site", "mn.yr"), model="within")
summary(fixed)

random <- plm(egg_total ~ T_suitability + R_suitability_hump + H_suitability + study_site, data=eggs.house, index=c("house", "mn.yr"), model="random")
summary(random)
# ranef(random)
# random <- plm(early_instar_total ~ T_suitability + R_suitability_hump + H_suitability + study_site, data=larvae.house, index=c("house", "mn.yr"), model="random")
# random <- plm(aedes_total ~ T_suitability + R_suitability_hump + H_suitability + study_site, data=adults.house, index=c("house", "mn.yr"), model="random")
# summary(random)

pcdtest(fixed, test = c("lm"))
pcdtest(fixed, test = c("cd"))

# test if pooling is needed 
znp <- pvcm(egg_total ~ T_suitability, data=eggs.house, index=c("house", "mn.yr"), model="within")
zplm <- plm(egg_total ~ T_suitability + R_suitability_hump + H_suitability, data=eggs.house, index=c("house", "mn.yr"), model="within")
pooltest(zplm,znp)
