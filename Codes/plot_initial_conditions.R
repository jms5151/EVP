# Visually compare model output given different initial conditions ------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries 
library(ggplot2)

# load data 
inits <- read.csv("Concatenated_Data/sensitivity_analyses/Initital_condition_simulations.csv", head=T)

# adjust dates and ic classes
inits$Date <- as.Date(inits$Date, "%Y-%m-%d")
inits$InitCond <- as.factor(inits$InitCond)

inits2 <- subset(inits, time < 100)

# plot initial conditions
ggplot() +
  geom_line(data = inits2, aes(x = Date, y = I, color = as.factor(InitCond), group = as.factor(InitCond))) +
  facet_wrap(~Site, ncol=2, scales = "free") +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Dengue cases (monthly)")




# plot trait variation -------------------------------------------------------------------
trhmean <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_TRH_final_model.csv", head=T, stringsAsFactors = F)
trhmean$Date <- as.Date(trhmean$Date, "%Y-%m-%d")
tv <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_with_trait_variation.csv", head=T, stringsAsFactors = F)
tv$Date <- as.Date(tv$Date, "%Y-%m-%d")
# tv <- tv[complete.cases(tv),]

library(tidyverse)

tv_ci <- tv %>%
  as_tibble() %>%
  group_by(Site, Date, time) %>% 
  summarise(cases = median(I),
            cases_low95 = quantile(I, 0.025),
            cases_high95 = quantile(I, 0.975),
            mozzies = median(M1+M2+M3),
            mozzies_low95 = quantile(M1+M2+M3, 0.025),
            mozzies_high95 = quantile(M1+M2+M3, 0.975)) %>%
  filter(Date > "2014-01-01")

max_vals <- tv_ci %>%
  group_by(Site) %>%
  summarise(Imax = max(cases),
            Mmax = max(mozzies))

# load case data
load("Concatenated_Data/case_data/merged_case_data.RData")
max_cases_observed <- cases %>%
  as_tibble() %>%
  group_by(Site) %>%
  summarise(max_cases_observed = max(denv_positive, na.rm=T)) %>%
  left_join(max_vals)

# load vector data
load("Concatenated_Data/vector_data/merged_vector_data.RData")

max_mozzies_and_cases <- vectors %>%
  as_tibble() %>%
  group_by(Site) %>%
  summarise(max_mozzies_observed = max(aedes_total, na.rm=T)) %>%
  left_join(max_cases_observed)
    

max_mozzies_and_cases$caseIF <- max_mozzies_and_cases$Imax/max_mozzies_and_cases$max_cases_observed
max_mozzies_and_cases$mozziesIF <- max_mozzies_and_cases$Mmax/max_mozzies_and_cases$max_mozzies_observed

# https://rpubs.com/MarkusLoew/226759

cases2 <- max_mozzies_and_cases %>%
  left_join(cases) %>%
  filter(Date > "2014-01-01")

vectors2 <- max_mozzies_and_cases %>%
  left_join(vectors) 

trhmean <- trhmean %>%
  filter(Date > "2014-01-01")
  
ggplot() +
  geom_point(data = cases2, aes(x = Date, y = denv_positive*caseIF/2)) +
  geom_line(data = cases2, aes(x = Date, y = denv_positive*caseIF/2)) +
  geom_line(data = trhmean, aes(x = Date, y = I)) +  
  geom_ribbon(data = tv_ci, aes(x = Date, ymin = cases_low95, ymax = cases_high95, alpha = 0.5)) +
  facet_wrap(~Site, ncol=2, scales = "free") +
  theme_bw() +
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(sec.axis = sec_axis(~./5, name = "Observed cases")) + # issue getting different scale for each 
  xlab("") +
  ylab("Modeled dengue cases")

trhmean$Mtot <- trhmean$M1 + trhmean$M2 + trhmean$M3

ggplot() +
  geom_point(data = vectors2, aes(x = Date, y = aedes_total*mozziesIF)) +
  geom_line(data = vectors2, aes(x = Date, y = aedes_total*mozziesIF)) +
  geom_line(data = trhmean, aes(x = Date, y = Mtot)) +
  geom_ribbon(data = tv_ci, aes(x = Date, ymin = mozzies_low95, ymax = mozzies_high95, alpha = 0.5)) +
  facet_wrap(~Site, ncol=2, scales = "free") +
  theme_bw() +
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(sec.axis = sec_axis(~./5000, name = "Observed mosquitoes")) + # issue getting different scale for each 
  xlab("") +
  ylab("Modeled mosquitoes")
