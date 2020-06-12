rm(list=ls()) #remove previous variable assignments

# load libraries
library(tidyverse)
library(plotly)
library(dplyr)

# load model simulations 
tv <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_with_trait_variation.csv", head=T, stringsAsFactors = F)
tv$Date <- as.Date(tv$Date, "%Y-%m-%d")

# format data and calculate 95% CI
tv_ci <- tv %>%
  as_tibble() %>%
  group_by(Site, Date, time) %>% 
  summarise(cases = median(I),
            cases_low95 = quantile(I, 0.025),
            cases_high95 = quantile(I, 0.975),
            mozzies = median(M1+M2+M3),
            mozzies_low95 = quantile(M1+M2+M3, 0.025),
            mozzies_high95 = quantile(M1+M2+M3, 0.975)) 

# load case data
load("Concatenated_Data/case_data/merged_case_data.RData")

# load vector data
load("Concatenated_Data/vector_data/merged_vector_data.RData")

# merge models and observations
tv2 <- tv_ci %>%
  left_join(cases) %>%
  left_join(vectors) %>%
  filter(Date > "2014-01-01")

# plot and save confirmed dengue cases
evpDENV <- function(dat){
  plot_ly() %>%
    add_trace(data=dat, x = ~Date, y = ~denv_positive, type = 'scatter', mode='line', color=I('black'))%>%
    add_trace(data=dat, x = ~Date, y = ~denv_positive, type = 'scatter', mode='line+markers', color=I('blue'), marker = list(size = 8, color = 'lightblue', line = list(color = 'black', width = 2)))%>%
    layout(showlegend = FALSE, yaxis = list(rangemode = "tozero", showline=T, linewidth=3), xaxis = list(rangemode = "tozero", showline=T, linewidth=2, showgrid=F))
}

dengue <- tv2 %>%
  group_by(Site) %>%
  do(map = evpDENV(.)) %>%
  subplot(nrows = 4) %>%
  layout(showlegend = FALSE, hovermode = FALSE)

export(dengue, file = "Figures/models_and_data/dengue_confirmed.pdf")

# plot and save aedes aegypti abundances
evpMOSQ <- function(dat){
  plot_ly() %>%
    add_trace(data=dat, x = ~Date, y = ~aedes_total, type = 'scatter', mode='line', color=I('black'))%>%
    add_trace(data=dat, x = ~Date, y = ~aedes_total, type = 'scatter', mode='line+markers', color=I('blue'), marker = list(size = 8, color = 'lightblue', line = list(color = 'black', width = 2)))%>%
    layout(showlegend = FALSE, yaxis = list(rangemode = "tozero", showline=T, linewidth=3), xaxis = list(rangemode = "tozero", showline=T, linewidth=2, showgrid=F))
}

mosquitoes <- tv2 %>%
  group_by(Site) %>%
  do(map = evpMOSQ(.)) %>%
  subplot(nrows = 4) %>%
  layout(showlegend = FALSE, hovermode = FALSE)

export(mosquitoes, file = "Figures/models_and_data/aedes.pdf")


# plot and save modeled dengue cases
evpModDENV <- function(dat){
  plot_ly() %>%
    add_trace(data=dat, x = ~Date, y = ~cases, type = 'scatter', mode = 'lines', color = I('black')) %>%
    add_ribbons(data = dat, x=~Date,
                ymin = ~cases_low95,
                ymax = ~cases_high95,
                color = I("gray75"),
                line = list(color = 'rgba(7, 164, 181, 0.05)')) %>%
    layout(showlegend = FALSE,
           yaxis = list(rangemode = "tozero", showline=T, linewidth=3),
           xaxis = list(rangemode = "tozero", showline=T, linewidth=2, showgrid=F))
}

ModDengue <- tv2 %>%
  group_by(Site) %>%
  do(map = evpModDENV(.)) %>%
  subplot(nrows = 4) %>%
  layout(showlegend = FALSE, hovermode = FALSE)

export(ModDengue, file = "Figures/models_and_data/dengue_modeled.pdf")

# plot and save modeled mosquitoes cases
evpModMosq <- function(dat){
  plot_ly() %>%
    add_trace(data=dat, x = ~Date, y = ~mozzies, type = 'scatter', mode = 'lines', color = I('black')) %>%
    add_ribbons(data = dat, x=~Date,
                ymin = ~mozzies_low95,
                ymax = ~mozzies_high95,
                color = I("gray75"),
                line = list(color = 'rgba(7, 164, 181, 0.05)')) %>%
    layout(showlegend = FALSE,
           yaxis = list(rangemode = "tozero", showline=T, linewidth=3),
           xaxis = list(rangemode = "tozero", showline=T, linewidth=2, showgrid=F))
}

ModMozzies <- tv2 %>%
  group_by(Site) %>%
  do(map = evpModMosq(.)) %>%
  subplot(nrows = 4) %>%
  layout(showlegend = FALSE, hovermode = FALSE)
