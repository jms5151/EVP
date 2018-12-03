# plot model simulations with vector and case data ----------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries 
library(plotly)

# load data 
simobs.data <- read.csv("Concatenated_Data/model_v_data.csv", head=T)

# set date 
simobs.data$Date <- as.Date(simobs.data$Date, "%Y-%m-%d")

# plotting function
data_overlay_sims_plot <- function(df, site, obsvar, obsname, simvarMean, simvarMin, simvarMax){
  df2 <- subset(df[,c("Site", "Date", simvarMean, simvarMin, simvarMax, obsvar)])
  df2 <- subset(df2, Site == site)
  firstDateIndex <- which(!is.na(df2[,obsvar]))[1]
  lastDateIndex <- which(!is.na(df2[,obsvar]))[length(which(!is.na(df2[,obsvar])))]
  df2 <- subset(df2, Date >= df2$Date[firstDateIndex] & Date <= df2$Date[lastDateIndex])
  Ylim <- max(df2[,obsvar], na.rm=T)
  obsvar<-obsvar
  simvarMean<-simvarMean
  simvarMin<-simvarMin
  simvarMax<-simvarMax
  obsname<-obsname
  overlayPlot <- plot_ly() %>%
    add_trace(data=df2, x = ~Date, y = ~df2[,obsvar], type = 'bar', name = paste0(obsname, ' (observed)'), color=I('lightblue'), opacity=0.7, yaxis = 'y2') %>%
    add_trace(data=df2, x = ~Date, y = ~df2[,simvarMean], type = 'scatter', mode = 'lines', name = 'Infected class', line = list(color = 'black', width = 3))%>%
    add_trace(data=df2, x = ~Date, y = ~df2[,simvarMin], type = 'scatter', name = 'Min I', mode = 'lines+markers', fill = 'tonexty', fillcolor='rgba(119,136,153,0.0)', line = list(color = 'transparent'))%>%
    add_trace(data=df2, x = ~Date, y = ~df2[,simvarMax], type = 'scatter', name = 'Max I', mode = 'lines+markers', fill = 'tonexty', fillcolor='rgba(119,136,153,0.2)', line = list(color = 'transparent')) %>%
    layout(title = paste(obsname, site, sep=' '), margin=list(l=60,r=60,b=100,t=50), showlegend = FALSE,
           yaxis = list(rangemode = "tozero", showline=T, linewidth=3, title = paste0(obsname, ' (modeled)')),
           xaxis = list(rangemode = "tozero", showline=T, linewidth=2, showgrid=F),
           yaxis2 = list(overlaying = "y", side='right', title = paste0(obsname, ' (observed)'), showline=F, showgrid=F, range = c(0, Ylim)))
  filename <- paste0("Figures/model_v_data_figs/", obsname, "_", site, ".png")
  export(overlayPlot, file = filename)
}

# set sites
sites <- unique(simobs.data$Site)

# plot dengue and adult mosquitoes for all sites
for (i in 1:length(sites)){
  data_overlay_sims_plot(simobs.data, sites[i], "denv_positive", "Dengue", "mean.I", "min.I", "max.I")
  data_overlay_sims_plot(simobs.data, sites[i], "aedes_total", "Adults", "mean.M", "min.M", "max.M")
}

# plot chikungunya and other mosquito life stages for kenya sites only
for (j in 1:4){
  data_overlay_sims_plot(simobs.data, sites[j], "chikv_positive", "Chikungunya", "mean.I", "min.I", "max.I")
  data_overlay_sims_plot(simobs.data, sites[j], "pupae_total", "Pupae", "mean.M", "min.M", "max.M")
  data_overlay_sims_plot(simobs.data, sites[j], "late_instar_total", "Pupae", "mean.M", "min.M", "max.M")
  data_overlay_sims_plot(simobs.data, sites[j], "early_instar_total", "Pupae", "mean.M", "min.M", "max.M")
  data_overlay_sims_plot(simobs.data, sites[j], "egg_total", "Pupae", "mean.M", "min.M", "max.M")
}

# line and dot plot -------------
# sims.obs.df<-simobs.data
# plot_ly() %>%
#   add_trace(data=sims.obs.df, x = ~Date, y = ~mean.I, type = 'scatter', mode = 'lines', name = 'Infected class', line = list(color = 'blue', width = 2))%>%
#   add_trace(data=sims.obs.df, x = ~Date, y = ~min.I, type = 'scatter', name = 'Min I', mode = 'lines+markers', fill = 'tonexty', fillcolor='rgba(119,136,153,0.0)', line = list(color = 'transparent'))%>%
#   add_trace(data=sims.obs.df, x = ~Date, y = ~max.I, type = 'scatter', name = 'Max I', mode = 'lines+markers', fill = 'tonexty', fillcolor='rgba(119,136,153,0.2)', line = list(color = 'transparent')) %>%
#   add_trace(data=sims.obs.df, x = ~Date, y = ~denv_positive, type = 'scatter', name = 'Observed cases', color=I('black'), marker = list(size = 10,color = 'rgba(255, 182, 193, .9)',line = list(color = 'rgba(152, 0, 0, .8)',width = 2)), yaxis = 'y2') %>%
#   layout(margin=list(l=60,r=60,b=100,t=50), showlegend = FALSE,
#          yaxis = list(rangemode = "tozero", showline=T, linewidth=3, title = "Infected people (modeled)"),
#          xaxis = list(rangemode = "tozero", showline=T, linewidth=2, showgrid=F),
#          yaxis2 = list(overlaying = "y", side='right', title = 'Disease cases (observed)', showline=F, showgrid=F, range = c(0, Ylim)))
