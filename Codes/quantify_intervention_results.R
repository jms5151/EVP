# summarize intervention simulations ------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(tidyverse)

# load functions
source("C:/Users/Jeremy/Box Sync/R_functions/standard_error.R")

# Intervention 1: reduce mosquitoes (e.g., spray larvicide) -------------------
# load data
spray_intervention <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_with_intervention_spray.csv", head=T, stringsAsFactors=F)

# summarize overall results for spray intervention
maxValuesSpray <- spray_intervention %>% 
  filter(time > 90) %>%
  group_by(Site, Intervention_strategy) %>%
  summarize(meanInfect = mean(I)) %>%
  summarize(maxVal = max(meanInfect))

spray_reductions <- spray_intervention %>% 
  filter(time > 90) %>%
  group_by(Site, Intervention_strategy) %>%
  summarize(meanInfect = mean(I)) %>% # , medInf = median(I), sdInf = sd(I)
  left_join(maxValuesSpray, by="Site")

spray_reductions$percReduc = round(((spray_reductions$maxVal-spray_reductions$meanInfect)/spray_reductions$maxVal)*100,2)
spray_reductions$Intervention_name <- "Reduce mosquito abundance"

# summarize results for spray intervention by date (to match with climate)
reductions <- spray_intervention %>% 
  filter(time > 90) %>%
  group_by(Site, SimID, Intervention_strategy, Intervention_date) %>%
  summarize(totalInf = sum(I)) # totalMosquitoes = sum(M1, M2, M3),
  
# Intervention 2: reduce mosquito carrying capactiy  --------------------------
# load data
reduceK_intervention <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_with_intervention_reduceK.csv", head=T, stringsAsFactors=F)

maxValuesK <- reduceK_intervention %>% 
  filter(time > 90) %>%
  group_by(Site, Intervention_strategy) %>%
  summarize(meanInfect = mean(I)) %>%
  summarize(maxVal = max(meanInfect))

K_reductions <- reduceK_intervention %>% 
  filter(time > 90) %>%
  group_by(Site, Intervention_strategy) %>%
  summarize(meanInfect = mean(I)) %>%
  left_join(maxValuesK, by="Site")

K_reductions$percReduc = round(((K_reductions$maxVal-K_reductions$meanInfect)/K_reductions$maxVal)*100,2)
K_reductions$Intervention_name <- "Reduce immature habitat"

# ggplot(reduceK_intervention, aes(fill=Intervention_strategy, y=meanInfect, x=Intervention_strategy)) +
#   geom_bar(position="dodge", stat="identity") +
#   facet_wrap(~Site)

# Intervention 3: reduce contact rate -----------------------------------------
# load data
reduceBiteRate_intervention <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_with_intervention_reduceBiteRate.csv", head=T, stringsAsFactors=F)

maxValuesBite <- reduceBiteRate_intervention %>%  
  filter(time > 90) %>%
  group_by(Site, Intervention_strategy) %>%
  summarize(meanInfect = mean(I)) %>%
  summarize(maxVal = max(meanInfect))

biteRate_reductions <- reduceBiteRate_intervention %>%  
  filter(time > 90) %>%
  group_by(Site, Intervention_strategy) %>%
  summarize(meanInfect = mean(I)) %>%
  left_join(maxValuesBite, by="Site")
            
biteRate_reductions$percReduc = round(((biteRate_reductions$maxVal-biteRate_reductions$meanInfect)/biteRate_reductions$maxVal)*100,2)
biteRate_reductions$Intervention_name <- "Reduce contact rate"

# combine intervention results ------------------------------------------------
# quantify reductions per population
source("Codes/SEI-SEIR_simulation_setup.R")
pop <- data.frame(sites, population)
colnames(pop) <- c("Site", "Population")
per_population_results <- bind_rows(spray_reductions, K_reductions, biteRate_reductions) %>%
  left_join(pop, by="Site")
per_population_results$diff <- per_population_results$maxVal - per_population_results$meanInfect
per_population_results$diff_per1000000 <- per_population_results$diff/(per_population_results$Population/100000)

mean(per_population_results$diff_per1000000[per_population_results$Intervention_name == "Reduce mosquito abundance" & per_population_results$Intervention_strategy == 0.1])
mean(per_population_results$diff_per1000000[per_population_results$Intervention_name == "Reduce immature habitat" & per_population_results$Intervention_strategy == 0.1])
mean(per_population_results$diff_per1000000[per_population_results$Intervention_name == "Reduce contact rate" & per_population_results$Intervention_strategy == 0.1])
max(per_population_results$diff_per1000000[per_population_results$Intervention_name == "Reduce immature habitat"|per_population_results$Intervention_name == "Reduce contact rate"])

# summarize by percentage reduction
all_interventions <- bind_rows(spray_reductions, K_reductions, biteRate_reductions) %>%
  filter(Intervention_strategy > 0) %>%
  group_by(Intervention_name, Intervention_strategy) %>%
  summarize(meanPercReduct = mean(percReduc), sePercReduct = se(percReduc))

save(all_interventions, file="Concatenated_Data/all_interventions.R")