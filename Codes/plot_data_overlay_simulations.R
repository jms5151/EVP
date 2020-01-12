# plot model simulations with vector and case data ----------------------------
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
# load("Concatenated_Data/case_data/merged_case_data.RData")

# load vector data
# load("Concatenated_Data/vector_data/merged_vector_data.RData")

# load data
load("Concatenated_Data/model_v_data_cases.RData")
load("Concatenated_Data/model_v_data_aedes.RData")

# load best model info
dengue_mods <- read.csv("Concatenated_Data/model_assessment/correlation_best_mod_dengue.csv", head=T, stringsAsFactors=F)
aedes_mods <- read.csv("Concatenated_Data/model_assessment/correlation_best_mod_aedes.csv", head=T, stringsAsFactors=F)

# test function
source("C:/Users/Jeremy/Box Sync/R_functions/mtexti.R")

# plot dengue cases
par(mfrow=c(4,2))

for (i in 1:8){ 
  mod2 <- subset(cases_and_mods, Rain_function == dengue_mods$Rain_function[i] & Site == dengue_mods$Site[i] & time > 90)
  mod2mod <- subset(mod2, !is.na(denv_positive))
  par(mar = c(4,4,2,4))
  with(mod2mod, plot(Date, I, type='l', cex=1.2, lwd=2, ylab='', xlab='', yaxt='n'))
  axis(side = 2, las=2)
  title(dengue_mods$Site[i], adj=0)
  par(new=T)
  with(mod2mod, plot(Date, denv_positive, axes=FALSE, ylab='', xlab='', type='o', pch=21, cex=1.2, lwd=2, col='darkblue', bg='lightblue'))
  axis(side = 4, las=2)
  # mtexti(side = 4, line = 3, 'Lab-confirmed cases')
  # legend("topright", legend=c('Predicted cases', "Observed cases")
  #        , pch=c(NA, 21), lty=c(1,1), lwd=c(2,2), bg=c(NA, 'lightblue')
  #        , col=c('black', 'darkblue'), text.col=c('black', 'darkblue')
  #        , bty = "n", cex=1.2)
  legend("topleft", legend=c(paste0("N=", nrow(mod2mod)), paste0("r=", dengue_mods$Dengue_corr[i]), paste0("Rain function=", dengue_mods$Rain_function[i])), bty='n', cex=1.2)
}

# plot mosquitoes
par(mfrow=c(4,2))

for (i in 1:8){ 
  mod2 <- subset(vectors_and_mods, Rain_function == aedes_mods$Rain_function[i] & Site == aedes_mods$Site[i] & time > 90)
  mod2mod <- subset(mod2, !is.na(aedes_total))
  par(mar = c(4,4,2,4))
  with(mod2mod, plot(Date, Mtot, type='l', cex=1.2, lwd=2, ylab='', xlab='', yaxt='n'))
  axis(side = 2, las=2)
  title(aedes_mods$Site[i], adj=0)
  par(new=T)
  with(mod2mod, plot(Date, aedes_total, axes=FALSE, ylab='', xlab='', type='o', pch=21, cex=1.2, lwd=2, col='darkblue', bg='lightblue'))
  axis(side = 4, las=2)
  if (i == 1){
    # mtexti(side = 4, line = 3, 'Lab-confirmed cases')
    legend("topright", legend=c('Predicted cases', "Observed cases")
           , pch=c(NA, 21), lty=c(1,1), lwd=c(2,2), bg=c(NA, 'lightblue')
           , col=c('black', 'darkblue'), text.col=c('black', 'darkblue')
           , bty = "n", cex=1.2)
  }
  legend("topleft", legend=c(paste0("N=", nrow(mod2mod)), paste0("r=", aedes_mods$Aedes_corr[i]), paste0("Rain function=", aedes_mods$Rain_function[i])), bty='n', cex=1.2)
}



# load final model for averaging
# models <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_TRH_final_model.csv", head=T, stringsAsFactors = F)
# models$Mtot <- models$M1 + models$M2 + models$M3
# models$Date <- as.Date(models$Date, "%Y-%m-%d")

# merge models and observations
merged_data <- tv_ci %>%
  left_join(cases) %>%
  left_join(vectors) %>%
  filter(Date > "2014-01-01")

merged_data <- as.data.frame(merged_data)
merged_data$Country <- ifelse(merged_data$Site=="Chulaimbo"|merged_data$Site=="Kisumu"|merged_data$Site=="Msambweni"|merged_data$Site=="Ukunda", "Kenya", "Ecuador")

# plotting function
data_overlay_sims_plot <- function(df, site, obsvar, obsname, simvarMean, CIlow, CIhigh){
  df2 <- subset(df[,c("Site", "Date", simvarMean, obsvar, CIlow, CIhigh)])
  df2 <- subset(df2, Site == site)
  firstDateIndex <- which(!is.na(df2[,obsvar]))[1]
  lastDateIndex <- which(!is.na(df2[,obsvar]))[length(which(!is.na(df2[,obsvar])))]
  df2 <- subset(df2, Date >= df2$Date[firstDateIndex] & Date <= df2$Date[lastDateIndex])
  df2 <- df2[order(df2$Date),]
  Ylim <- max(df2[,obsvar], na.rm=T) + 2
  obsvar<-obsvar
  simvarMean<-simvarMean
  obsname<-obsname
  overlayPlot <- plot_ly() %>%
    add_trace(data=df2, x = ~Date, y = ~df2[,obsvar], type = 'scatter', mode='line', color=I('black'), yaxis='y2')%>%
    add_trace(data=df2, x = ~Date, y = ~df2[,obsvar], type = 'scatter', mode='line+markers', color=I('black'), marker = list(size = 10, color = 'lightblue', line = list(color = 'black', width = 2)), yaxis='y2')%>%
    add_trace(data=df2, x = ~Date, y = ~df2[,simvarMean], type = 'scatter', mode = 'lines', name = 'Infected class', line = list(color = 'gray', width = 2))%>%
    add_ribbons(data = df2, x=~Date, ymin = ~df2[,CIlow], ymax = ~df2[,CIhigh], color = I("gray75"), line = list(color = 'rgba(7, 164, 181, 0.05)')) %>%
    layout(title = paste(obsname, site, sep=' '), margin=list(l=60,r=60,b=100,t=50), showlegend = FALSE,
           yaxis = list(showline=T, linewidth=3, title = paste0(obsname, ' (modeled)')),
           xaxis = list(rangemode = "tozero", showline=T, linewidth=2, showgrid=F),
           yaxis2 = list(overlaying = "y", side='right', title = paste0(obsname, ' (observed)'), showline=F, showgrid=F, range = c(0, 40)))
  # filename <- paste0("Figures/model_v_data_figs_dots/", obsname, "_", site, ".png")
  filename <- paste0("Figures/model_v_data_figs_pdfs/", obsname, "_", site, ".pdf")
  export(overlayPlot, file = filename)
}

data_overlay_sims_plot(merged_data, "Chulaimbo", "denv_positive", "Dengue", "cases", "cases_low95", "cases_high95")

# load library 
library(webshot)
library(orca)
library(plotly)

# set sites
sites <- unique(merged_data$Site)
countries <- c(rep(c("Kenya", "Ecuador"),4))

# plot with CI
for (i in 1:length(sites)){
  data_overlay_sims_plot(merged_data, sites[i], "denv_positive", "Dengue", "cases", "cases_low95", "cases_high95")
  data_overlay_sims_plot(merged_data, sites[i], "aedes_total", "Aedes_aegypti", "mozzies", "mozzies_low95", "mozzies_high95")
  # if (countries[i]=="Ecuador" & sum(!is.na(merged_data$denv_positive_clinically_diagnosed[merged_data$Site==sites[i]])) > 0){
  #   data_overlay_sims_plot(merged_data, sites[i], "denv_positive_clinically_diagnosed", "Dengue_clinically_diagnosed", "cases", "cases_low95", "cases_high95")
  # }
  if (countries[i] == "Kenya"){
  data_overlay_sims_plot(merged_data, sites[i], "aedes_total2", "Aedes_aegypti2", "mozzies", "mozzies_low95", "mozzies_high95")
  data_overlay_sims_plot(merged_data, sites[i], "aedes_total_bg", "Aedes_aegypti_bg", "mozzies", "mozzies_low95", "mozzies_high95")
  data_overlay_sims_plot(merged_data, sites[i], "pupae_total", "Pupae", "mozzies", "mozzies_low95", "mozzies_high95")
  data_overlay_sims_plot(merged_data, sites[i], "late_instar_total", "Late_instar", "mozzies", "mozzies_low95", "mozzies_high95")
  data_overlay_sims_plot(merged_data, sites[i], "early_instar_total", "Early_instar", "mozzies", "mozzies_low95", "mozzies_high95")
  data_overlay_sims_plot(merged_data, sites[i], "egg_total_adjusted", "Eggs", "mozzies", "mozzies_low95", "mozzies_high95")
  }
  cat(paste0("finished ", sites[i], "\n"))
}

# Overlay denuge, chikungunya, and zika for Machala
load("Concatenated_Data/CDC_Zika_El_Oro_Total.RData")
cdcZika2$Site <- "Machala"
cases2 <- merge(cases, cdcZika2, by=c("Site", "Date"), all=T)
cases3 <- cases2[,c("denv_positive", "chikv_positive", "confirmed_cases_Zika")] #"chikv_positive_clinically_diagnosed", 
cases3$arboviruses <- rowSums(cases3, na.rm=T)
cases3$Date <- cases2$Date
cases3$Site <- cases2$Site

# merged_data <- merge(models[,c("Site", "Date", "I")], cases3, by=c("Site", "Date"), all=T)
df2 <- tv_ci %>%
  left_join(cases3) %>%
  filter(Date > "2014-01-01" & Site == "Machala")
df2 <- data.frame(df2)
df2$zikv_positive <- df2$confirmed_cases_Zika

overlayPlot <- plot_ly() %>%
  add_trace(data=df2, x = ~Date, y = ~df2[,'denv_positive'], type = 'scatter', name= '', mode='line', color=I('blue'), yaxis='y2')%>%
  add_trace(data=df2, x = ~Date, y = ~df2[,'denv_positive'], type = 'scatter', name= 'Denv', mode='line+markers', color=I('blue'), marker = list(size = 10, color = 'lightblue', line = list(color = 'black', width = 2)), yaxis='y2')%>%
  add_trace(data=df2, x = ~Date, y = ~df2[,'chikv_positive'], type = 'scatter', name= '', mode='line', color=I('orange'), yaxis='y3')%>%
  add_trace(data=df2, x = ~Date, y = ~df2[,'chikv_positive'], type = 'scatter', name= 'Chikv', mode='line+markers', color=I('orange'), marker = list(size = 10, color = 'yellow', line = list(color = 'black', width = 2)), yaxis='y3')%>%
  add_trace(data=df2, x = ~Date, y = ~df2[,'zikv_positive'], type = 'scatter', name= '', mode='line', color=I('red'), yaxis='y4')%>%
  add_trace(data=df2, x = ~Date, y = ~df2[,'zikv_positive'], type = 'scatter', name='Zikv', mode='line+markers', color=I('red'), marker = list(size = 10, color = 'pink', line = list(color = 'black', width = 2)), yaxis='y4')%>%
  add_trace(data=df2, x = ~Date, y = ~df2[,'cases'], type = 'scatter', mode = 'lines', name = 'Modeled infected class', line = list(color = 'gray', width = 2))%>%
  add_ribbons(data=df2, x=~Date, ymin = ~cases_low95, ymax = ~cases_high95, color = I("gray75"), line = list(color = 'rgba(7, 164, 181, 0.05)')) %>%
  layout(margin=list(l=60,r=100,b=100,t=50, showlegend = FALSE), #legend = list(x = 0.01, y = 0.99),
         yaxis = list(rangemode = "tozero", showline=T, linewidth=3, title = 'I (modeled)'),
         xaxis = list(rangemode = "tozero", showline=T, linewidth=2, showgrid=F),
         yaxis2 = list(overlaying = "y", side='right', title = 'Laboratory-confirmed cases', showline=F, showgrid=F, range = c(0, 200), color='blue'),
         yaxis3 = list(overlaying = "y", side='right', title = '', showline=F, showgrid=F, range = c(0, 180), color='orange'),
         yaxis4 = list(overlaying = "y", side='right', title = '', showline=F, showgrid=F, range = c(0, 550), color='red'))
export(overlayPlot, file = "Manuscript/overlay_plot_machala.pdf")

# huaquillas
df2 <- tv_ci %>%
  left_join(cases3) %>%
  filter(Date > "2015-05-01" & Date < "2018-11-01" & Site == "Huaquillas")
df2 <- data.frame(df2)

overlayPlot2 <- plot_ly() %>%
  add_trace(data=df2, x = ~Date, y = ~df2[,'denv_positive'], type = 'scatter', name= '', mode='line', color=I('blue'), yaxis='y2')%>%
  add_trace(data=df2, x = ~Date, y = ~df2[,'denv_positive'], type = 'scatter', name= 'Denv', mode='line+markers', color=I('blue'), marker = list(size = 10, color = 'lightblue', line = list(color = 'black', width = 2)), yaxis='y2')%>%
  add_trace(data=df2, x = ~Date, y = ~df2[,'chikv_positive'], type = 'scatter', name= '', mode='line', color=I('orange'), yaxis='y3')%>%
  add_trace(data=df2, x = ~Date, y = ~df2[,'chikv_positive'], type = 'scatter', name= 'Chikv', mode='line+markers', color=I('orange'), marker = list(size = 10, color = 'yellow', line = list(color = 'black', width = 2)), yaxis='y3')%>%
  add_trace(data=df2, x = ~Date, y = ~df2[,'cases'], type = 'scatter', mode = 'lines', name = 'Modeled infected class', line = list(color = 'gray', width = 2))%>%
  add_ribbons(data=df2, x=~Date, ymin = ~cases_low95, ymax = ~cases_high95, color = I("gray75"), line = list(color = 'rgba(7, 164, 181, 0.05)')) %>%
  layout(margin=list(l=60,r=100,b=100,t=50, showlegend = FALSE), #legend = list(x = 0.01, y = 0.99),
         yaxis = list(rangemode = "tozero", showline=T, linewidth=3, title = 'I (modeled)'),
         xaxis = list(rangemode = "tozero", showline=T, linewidth=2, showgrid=F),
         yaxis2 = list(overlaying = "y", side='right', title = 'Laboratory-confirmed cases', showline=F, showgrid=F, range = c(0, 50), color='blue'),
         yaxis3 = list(overlaying = "y", side='right', title = '', showline=F, showgrid=F, range = c(0, 55), color='orange'))
export(overlayPlot2, file = "Manuscript/overlay_plot_huaquillas.pdf")



# df <- subset(merged_data, Site == "Zaruma" & Date >= '2014-02-01')
# df2 <- df[,c('Date', 'I', "denv_positive", "chikv_positive")]
# 
# plot_ly() %>%
#   add_trace(data=df2, x = ~Date, y = ~df2[,'denv_positive'], type = 'scatter', name= '', mode='line', color=I('blue'), yaxis='y2')%>%
#   add_trace(data=df2, x = ~Date, y = ~df2[,'denv_positive'], type = 'scatter', name= 'Denv', mode='line+markers', color=I('blue'), marker = list(size = 10, color = 'lightblue', line = list(color = 'black', width = 2)), yaxis='y2')%>%
#   add_trace(data=df2, x = ~Date, y = ~df2[,'chikv_positive'], type = 'scatter', name= '', mode='line', color=I('orange'), yaxis='y3')%>%
#   add_trace(data=df2, x = ~Date, y = ~df2[,'chikv_positive'], type = 'scatter', name= 'Chikv', mode='line+markers', color=I('orange'), marker = list(size = 10, color = 'yellow', line = list(color = 'black', width = 2)), yaxis='y3')%>%
#   add_trace(data=df2, x = ~Date, y = ~df2[,'I'], type = 'scatter', mode = 'lines', name = 'Modeled infected class', line = list(color = 'gray', width = 2))%>%
#   layout(margin=list(l=60,r=100,b=100,t=50), showlegend = FALSE,
#          yaxis = list(rangemode = "tozero", showline=T, linewidth=3, title = 'I (modeled)'),
#          xaxis = list(rangemode = "tozero", showline=T, linewidth=2, showgrid=F),
#          yaxis2 = list(overlaying = "y", side='right', title = 'Laboratory-confirmed cases', showline=F, showgrid=F, range = c(0, max(df2$denv_positive)+2), color='blue'),
#          yaxis3 = list(overlaying = "y", side='right', title = '', showline=F, showgrid=F, range = c(0, max(df2$chikv_positive)+2), color='orange'))
