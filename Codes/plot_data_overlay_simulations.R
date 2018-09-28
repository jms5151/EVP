# plot model simulations with vector and case data ----------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries --------------------------------------------------------------
library(plotly)

# load data ----------------------------------------------------
simobs.data <- read.csv("Kenya/Concatenated_Data/model_v_data.csv", head=T)

# set date --------------------------------------------------------------------
simobs.data$Date <- as.Date(simobs.data$Date, "%Y-%m-%d")

# Plot vectors ----------------------------------------------------------------
sites <- c("Chulaimbo", "Kisumu", "Msambweni", "Ukunda")
Yvar <- c("aedes_total", "pupae_total", "late_instar_total", "early_instar_total", "egg_total", "denv_positive", "chikv_positive")
names <- c("Adults", "Pupae", "Late_instars", "Early_instars", "Eggs", "denv", "chikv")

for (i in 1:length(Yvar)){
  for (j in 1:length(sites)){
    sims.obs.df <- subset(simobs.data, Site == sites[j] & Date > "2014-01-01")
    sims.obs.df$total <- sims.obs.df[,Yvar[i]]
    Ylim <- max(sims.obs.df$total, na.rm=T)
    if (names[i] == "denv" | names[i] == "chikv"){
      sims.obs.df$positive <- sims.obs.df[,Yvar[i]]
      Ylim <- max(sims.obs.df$total, na.rm=T)*4
      overlayPlot <- plot_ly() %>%
        add_trace(data=sims.obs.df, x = ~Date, y = ~positive, type = 'bar', name = 'Mosquitoes (observed)', color=I('lightblue'), opacity=0.7, yaxis = 'y2') %>%
        add_trace(data=sims.obs.df, x = ~Date, y = ~mean.I, type = 'scatter', mode = 'lines', name = 'Infected class', line = list(color = 'black', width = 3))%>%
        add_trace(data=sims.obs.df, x = ~Date, y = ~min.I, type = 'scatter', name = 'Min I', mode = 'lines+markers', fill = 'tonexty', fillcolor='rgba(119,136,153,0.0)', line = list(color = 'transparent'))%>%
        add_trace(data=sims.obs.df, x = ~Date, y = ~max.I, type = 'scatter', name = 'Max I', mode = 'lines+markers', fill = 'tonexty', fillcolor='rgba(119,136,153,0.2)', line = list(color = 'transparent')) %>%
        layout(title = paste(names[i], sites[j], sep=' '), margin=list(l=60,r=60,b=100,t=50), showlegend = FALSE,
               yaxis = list(rangemode = "tozero", showline=T, linewidth=3, title = "Infected people (modeled)"),
               xaxis = list(rangemode = "tozero", showline=T, linewidth=2, showgrid=F),
               yaxis2 = list(overlaying = "y", side='right', title = 'Disease cases (observed)', showline=F, showgrid=F, range = c(0, Ylim)))
      filename <- paste0("Kenya/Figures/SEI-SEIR-Output/model_v_data/", names[i], "_", sites[j], "_model_v_cases.png")
      export(overlayPlot, file = filename)
    } else {
      overlayPlot <- plot_ly() %>%
        add_trace(data=sims.obs.df, x = ~Date, y = ~total, type = 'bar', name = 'Mosquitoes (observed)', color=I('lightblue'), opacity=0.7, yaxis = 'y2') %>%
        add_trace(data=sims.obs.df, x = ~Date, y = ~mean.M, type = 'scatter', mode = 'lines', name = 'Mosquitoes', line = list(color = 'black', width = 3))%>%
        add_trace(data=sims.obs.df, x = ~Date, y = ~min.M, type = 'scatter', name = 'Min M', mode = 'lines+markers', fill = 'tonexty', fillcolor='rgba(119,136,153,0.0)', line = list(color = 'transparent'))%>%
        add_trace(data=sims.obs.df, x = ~Date, y = ~max.M, type = 'scatter', name = 'Max M', mode = 'lines+markers', fill = 'tonexty', fillcolor='rgba(119,136,153,0.2)', line = list(color = 'transparent')) %>%
        layout(title = paste(sites[j], names[i], sep=' '), margin=list(l=60,r=60,b=100,t=50), showlegend = FALSE,
               yaxis = list(rangemode = "tozero", showline=T, linewidth=3, title = "Mosquitoes (modeled)"),
               xaxis = list(rangemode = "tozero", showline=T, linewidth=2, showgrid=F),
               yaxis2 = list(overlaying = "y", side='right', title = 'Mosquitoes (observed)', showline=F, showgrid=F, range = c(0, Ylim)))
      filename <- paste0("Kenya/Figures/SEI-SEIR-Output/model_v_data/", names[i], "_", sites[j], "_model_v_traps.png")
      export(overlayPlot, file = filename)
    }
  }
}

# line and dot plot -------------
# overlayPlot <- plot_ly() %>%
#   add_trace(data=sims.obs.df, x = ~Date, y = ~mean.I, type = 'scatter', mode = 'lines', name = 'Infected class', line = list(color = 'blue', width = 2))%>%
#   add_trace(data=sims.obs.df, x = ~Date, y = ~se.I.Min, type = 'scatter', name = 'Min I', mode = 'lines+markers', fill = 'tonexty', fillcolor='rgba(119,136,153,0.0)', line = list(color = 'transparent'))%>%
#   add_trace(data=sims.obs.df, x = ~Date, y = ~se.I.Max, type = 'scatter', name = 'Max I', mode = 'lines+markers', fill = 'tonexty', fillcolor='rgba(119,136,153,0.2)', line = list(color = 'transparent')) %>%
#   add_trace(data=sims.obs.df, x = ~Date, y = ~positive, type = 'scatter', name = 'Observed cases', color=I('black'), marker = list(size = 10,color = 'rgba(255, 182, 193, .9)',line = list(color = 'rgba(152, 0, 0, .8)',width = 2)), yaxis = 'y2') %>%
#   layout(title = paste(arbov.names[i], sites[j], sep=' '), margin=list(l=60,r=60,b=100,t=50), showlegend = FALSE,
#          yaxis = list(rangemode = "tozero", showline=T, linewidth=3, title = "Infected people (modeled)"),
#          xaxis = list(rangemode = "tozero", showline=T, linewidth=2, showgrid=F),
#          yaxis2 = list(overlaying = "y", side='right', title = 'Disease cases (observed)', showline=F, showgrid=F, range = c(0, Ylim)))
# filename <- paste0("Kenya/Figures/Case_data/cases_v_model/", arbov.names[i], "_", sites[j], "_model_v_cases.png")