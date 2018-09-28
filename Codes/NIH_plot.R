# Plot with climate, vectors, case data, and simulations for NIH grant proposal -------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(plotly)
library(plyr)

# load and plot climate data ----------------------------------------------------------------------
climateData <- read.csv("Kenya/Concatenated_Data/climate/gapfilled_climate_data.csv", head=T)
climateData$Date <- as.Date(climateData$Date, "%Y-%m-%d")

# load SEI-SEIR simulation -------------------------------------------------
site <- "Kisumu"
site <- "Ukunda"

fileName <- paste0("Kenya/Concatenated_Data/SEI-SEIR/SEI-SEIR_Simulations_TempOnly_", site, ".csv")

seiseir_temp <- read.csv(fileName, head=T)
seiseir_temp$Date <- as.Date(seiseir_temp$Date, "%Y-%m-%d")
seiseir_temp$mon.yr <- substr(seiseir_temp$Date, 1, 7)

sei <-  ddply(seiseir_temp, .(mon.yr),
                summarise,
                infected = mean(na.omit(I)),
                mosquitoes = mean(totalMosquitoPop))

# load case data -----------------------------------------------------------
aic_cases <- read.csv("Kenya/Concatenated_Data/aic_timeline.csv", head=T, stringsAsFactors = F)
aic_cases[,c("visit_a_int_date", "infected.t0", "infected.t1")] <- lapply(aic_cases[,c("visit_a_int_date", "infected.t0", "infected.t1")], as.Date)
aic_cases$mon.yr <- format(aic_cases$infected.t0, "%Y-%m")
aic_cases$mon <- format(aic_cases$infected.t0, "%m")
aic_cases <- subset(aic_cases, id_site == "Kisumu")
# aic_cases <- subset(aic_cases, id_site == "Ukunda")

aic.df <- ddply(aic_cases, .(mon.yr),
                summarise,
                denv = sum(visit_a_infected_denv == 1, na.rm=T),
                chikv = sum(visit_a_infected_chikv == 1, na.rm=T))

aic.df <- merge(aic.df, sei[,c("mon.yr", "infected")], by="mon.yr")

# load vector data -----------------------------------------------------------
# ovitrap
# ovitrap <- read.csv("Kenya/Concatenated_Data/vector/redcap_ovitrap.csv", head=T)
# ovitrap$date_collected <- as.Date(ovitrap$date_collected, "%Y-%m-%d")
# ovitrap$mon.yr <- format(ovitrap$date_collected, "%Y-%m")
# # ovitrap <- subset(ovitrap, study_site == 1) #1=Ukunda, 4=Kisumu
# ovitrap <- subset(ovitrap, study_site == 4)
# 
# ovi <- ddply(ovitrap, .(mon.yr),
#              summarise,
#              eggs = sum(egg_count_ovitrap, na.rm=T))
# 
# ovi <- merge(ovi, sei[,c("mon.yr", "mosquitoes")], by="mon.yr")

# prokopack
prokopack <- read.csv("Kenya/Concatenated_Data/vector/redcap_prokopack.csv", head=T)
prokopack$date_collected <- as.Date(prokopack$date_collected, "%Y-%m-%d")
prokopack$mon.yr <- format(prokopack$date_collected, "%Y-%m")
prokopack <- subset(prokopack, study_site == 1) #1=Ukunda, 4=Kisumu
# prokopack <- subset(prokopack, study_site == 4)

prokopack2 <- ddply(prokopack, .(mon.yr),
             summarise,
             Mosquitoes = sum(mosquitoes, na.rm=T))

prokopack2 <- merge(prokopack2, sei[,c("mon.yr", "mosquitoes")], by="mon.yr")

# plot data -----------------------------------------------------------------
siteTemp <- climateData$GF_Kisumu_mean_temp
siteRain <- climateData$GF_Kisumu_rain
siteRH <- climateData$GF_Kisumu_humidity

siteTemp <- climateData$GF_Ukunda_mean_temp
siteRain <- climateData$GF_Ukunda_rain
siteRH <- climateData$GF_Ukunda_humidity

temp <- plot_ly() %>%
  add_trace(data=climateData, x = ~Date, y = ~siteTemp, type = 'scatter', mode = 'lines', color=I('darkred'))%>%
  layout(yaxis = list(side = 'left', title = 'Mean temperature (C)', showgrid = FALSE, showline=TRUE), xaxis=list(showline=TRUE, showgrid = FALSE))

rain <- plot_ly() %>%
  add_trace(data=climateData, x = ~Date, y = ~siteRain, type = 'scatter', mode = 'lines', color=I('blue'))%>%
  layout(yaxis = list(side = 'left', title = 'Total rainfall (mm)', showgrid = FALSE, showline=TRUE), xaxis=list(showline=TRUE, showgrid = FALSE, title=""))

humidity <- plot_ly() %>%
  add_trace(data=climateData, x = ~Date, y = ~siteRH, type = 'scatter', mode = 'lines', color=I('orange'))%>%
  layout(yaxis = list(side = 'left', title = 'Relative humidity', showgrid = FALSE, showline=TRUE), xaxis=list(showline=TRUE, showgrid = FALSE))

subplot(temp,rain,humidity, nrows = 3, shareX = T, titleX = FALSE, titleY = TRUE)%>%layout(showlegend= FALSE)

# eggPlot <- plot_ly() %>%
#   add_trace(data=ovi, x = ~mon.yr, y = ~mosquitoes, type = 'scatter', mode='lines', color=I('black'))%>%
#   add_trace(data=ovi, x = ~mon.yr, y = ~eggs, type = 'bar', color=I('lightblue'), opacity=0.7, yaxis = 'y2')%>%
#   layout(margin=list(l=60,r=60,b=100,t=50),
#          yaxis = list(side = 'left', title = 'Modeled mosquitoes', showgrid = FALSE, showline=TRUE), 
#          xaxis=list(showline=FALSE, showgrid = FALSE, title = ''),
#          yaxis2 = list(overlaying = "y", side='right', title = 'Eggs'), showlegend = FALSE)

adults <- plot_ly() %>%
  add_trace(data=prokopack2, x = ~mon.yr, y = ~mosquitoes, type = 'scatter', mode='lines', color=I('black'))%>%
  add_trace(data=prokopack2, x = ~mon.yr, y = ~Mosquitoes, type = 'bar', color=I('lightblue'), opacity=0.7, yaxis = 'y2')%>%
  layout(margin=list(l=60,r=60,b=100,t=50),
         yaxis = list(side = 'left', title = 'Modeled mosquitoes', showgrid = FALSE, showline=TRUE), 
         xaxis=list(showline=FALSE, showgrid = FALSE, title = ''),
         yaxis2 = list(overlaying = "y", side='right', title = 'Observed mosquitoes'), showlegend = FALSE)

denvPlot <- plot_ly() %>%
  add_trace(data=aic.df, x = ~mon.yr, y = ~infected, type = 'scatter', mode='lines', color=I('black'))%>%
  add_trace(data=aic.df, x = ~mon.yr, y = ~denv, type = 'bar', color=I('darkred'), opacity=0.7, yaxis = 'y2')%>%
  layout(margin=list(l=60,r=60,b=100,t=50),
         yaxis = list(side = 'left', title = 'Modeled disease cases', showgrid = FALSE, showline=TRUE), 
         xaxis=list(showline=FALSE, showgrid = FALSE, title = ''),
         yaxis2 = list(overlaying = "y", side='right', title = 'Dengue positive'), showlegend = FALSE)

# chikvPlot <- plot_ly() %>%
#   add_trace(data=aic.df, x = ~mon.yr, y = ~infected, type = 'scatter', mode='lines', color=I('black'))%>%
#   add_trace(data=aic.df, x = ~mon.yr, y = ~chikv, type = 'bar', color=I('darkred'), opacity=0.7, yaxis = 'y2')%>%
#   layout(margin=list(l=60,r=60,b=100,t=50),
#          yaxis = list(side = 'left', title = 'Modeled disease cases', showgrid = FALSE, showline=TRUE), 
#          xaxis=list(showline=FALSE, showgrid = FALSE, title = ''),
#          yaxis2 = list(overlaying = "y", side='right', title = 'Chikungunya positive'), showlegend = FALSE)

subplot(temp,rain,humidity, adults, denvPlot, nrows = 3, titleY = TRUE)%>%layout(showlegend= FALSE)
