# plot AIC disease cases over time --------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load library
library(plotly)

# read in data
# source("Codes/concat_cohort_case_data_from_redcap.R") # starts from downloading data
cases <- read.csv("Kenya/Concatenated_Data/aic_cases_by_month.csv", head=T)

# plot dengue and chikungunya cases by site over time ------------------------
sites <- c("Chulaimbo", "Kisumu", "Msambweni", "Ukunda")
diseases <- c("denv", "chikv")

for (i in 1:length(sites)){
  diseaseDF <- subset(cases, Site == sites[i])
  for (j in 1:length(diseases)){
    diseaseDF$positive <- diseaseDF[,paste0(diseases[j], "_positive")]
    yaxisTitle <- paste0(toupper(diseases[j]), ' cases')
    caseplots <- plot_ly() %>%
      add_trace(data=diseaseDF, x = ~Year.Month, y = ~positive, type = 'bar', color=I('black'))%>%
    layout(margin=list(l=60,r=60,b=100,t=50), title = sites[i], yaxis = list(title = yaxisTitle, showline=TRUE), xaxis = list(title=""))
    filename <- paste0("Kenya/Figures/Case_data/AIC_monthly_", diseases[j], "_cases_", sites[i], ".png")
    export(caseplots, file = filename)
  }
}


library(ggplot2)
library(plyr)
library(webshot)

kisumu_seiseir_temp <- read.csv("Kenya/Concatenated_Data/SEI-SEIR/SEI-SEIR_Simulations_TempOnly_Kisumu.csv", head=T)
kisumu_seiseir_temp$Date <- as.Date(kisumu_seiseir_temp$Date, "%Y-%m-%d")
kisumu_seiseir_temp$mon.yr <- substr(kisumu_seiseir_temp$Date, 1, 7)

chulaimbo_seiseir_temp <- read.csv("Concatenated_Data/SEI-SEIR/SEI-SEIR_Simulations_TempOnly_Chulaimbo.csv", head=T)
chulaimbo_seiseir_temp$Date <- as.Date(chulaimbo_seiseir_temp$Date, "%Y-%m-%d")
chulaimbo_seiseir_temp$mon.yr <- substr(chulaimbo_seiseir_temp$Date, 1, 7)

msambweni_seiseir_temp <- read.csv("Concatenated_Data/SEI-SEIR/SEI-SEIR_Simulations_TempOnly_Msambweni.csv", head=T)
msambweni_seiseir_temp$Date <- as.Date(msambweni_seiseir_temp$Date, "%Y-%m-%d")
msambweni_seiseir_temp$mon.yr <- substr(msambweni_seiseir_temp$Date, 1, 7)

ukunda_seiseir_temp <- read.csv("Concatenated_Data/SEI-SEIR/SEI-SEIR_Simulations_TempOnly_Ukunda.csv", head=T)
ukunda_seiseir_temp$Date <- as.Date(ukunda_seiseir_temp$Date, "%Y-%m-%d")
ukunda_seiseir_temp$mon.yr <- substr(ukunda_seiseir_temp$Date, 1, 7)

seiCh <-  ddply(chulaimbo_seiseir_temp, .(mon.yr),
                summarise,
                infected = mean(na.omit(I)))

seiKi <-  ddply(kisumu_seiseir_temp, .(mon.yr),
                summarise,
                infected = mean(na.omit(I)))

seiMs <-  ddply(msambweni_seiseir_temp, .(mon.yr),
                summarise,
                infected = mean(na.omit(I)))

seiUk <-  ddply(ukunda_seiseir_temp, .(mon.yr),
                summarise,
                infected = mean(na.omit(I)))


aic_cases <- read.csv("Kenya/Concatenated_Data/aic_timeline.csv", head=T, stringsAsFactors = F)
aic_cases[,c("visit_a_int_date", "infected.t0", "infected.t1")] <- lapply(aic_cases[,c("visit_a_int_date", "infected.t0", "infected.t1")], as.Date)
aic_cases$mon.yr <- format(aic_cases$infected.t0, "%Y-%m")
aic_cases$mon <- format(aic_cases$infected.t0, "%m")

aic.df <- ddply(aic_cases, .(mon.yr),
                summarise,
                denv = sum(visit_a_infected_denv == 1, na.rm=T),
                chikv = sum(visit_a_infected_chikv == 1, na.rm=T))

aic.monthly.df <- ddply(aic_cases, .(mon),
                summarise,
                denv = sum(visit_a_infected_denv == 1, na.rm=T),
                chikv = sum(visit_a_infected_chikv == 1, na.rm=T))

# plot total cases by month-year ------
plot_ly(aic.df, x = ~mon.yr, y = ~denv, type = 'bar',  color = I("black")) %>% layout(title = "", xaxis = list(title = "Time"), yaxis = list(title = "Dengue cases"), margin = list(b = 80)) 
plot_ly(aic.df, x = ~mon.yr, y = ~chikv, type = 'bar',  color = I("black")) %>% layout(title = "", xaxis = list(title = "Time"), yaxis = list(title = "Chikungunya cases"), margin = list(b = 80))

# plot total cases by month ------
plot_ly(aic.monthly.df, x = ~mon, y = ~denv, type = 'bar',  color = I("black")) %>% layout(title = "", xaxis = list(title = "Month"), yaxis = list(title = "Dengue cases"), margin = list(b = 80)) 
plot_ly(aic.monthly.df, x = ~mon, y = ~chikv, type = 'bar',  color = I("black")) %>% layout(title = "", xaxis = list(title = "Month"), yaxis = list(title = "Chikungunya cases"), margin = list(b = 80))

# plot model v cases by site ------
aic.df2 <- ddply(aic_cases, .(mon.yr, id_site),
                summarise,
                denv = sum(visit_a_infected_denv == 1, na.rm=T),
                chikv = sum(visit_a_infected_chikv == 1, na.rm=T))

for (i in 1:4){
  if (i==1){
    seiData <- seiCh
    caseData <- subset(aic.df2, id_site == "Chulaimbo")
    cases.df <- merge(seiData, caseData, by="mon.yr", all=T)
    site <- "Chulaimbo"
  } else if (i==2){
    seiData <- seiKi
    caseData <- subset(aic.df2, id_site == "Kisumu")
    cases.df <- merge(seiData, caseData, by="mon.yr", all=T)
    site <- "Kisumu"
  } else if (i==3){
    seiData <- seiMs
    caseData <- subset(aic.df2, id_site == "Msambweni")
    cases.df <- merge(seiData, caseData, by="mon.yr", all=T)
    site <- "Msambweni"
  } else {
    seiData <- seiUk
    caseData <- subset(aic.df2, id_site == "Ukunda")
    cases.df <- merge(seiData, caseData, by="mon.yr", all=T)
    site <- "Ukunda"
  } 
  denvPlot <- plot_ly() %>%
    add_trace(data=cases.df, x = ~mon.yr, y = ~infected, type = 'scatter', mode='lines', color=I('black'))%>%
    add_trace(data=cases.df, x = ~mon.yr, y = ~denv, type = 'bar', color=I('darkred'), opacity=0.7, yaxis = 'y2')%>%
    layout(margin=list(l=60,r=60,b=100,t=50),
           yaxis = list(side = 'left', title = 'Modeled dengue cases', showgrid = FALSE, showline=TRUE), 
           xaxis=list(showline=FALSE, showgrid = FALSE),
           yaxis2 = list(overlaying = "y", side='right', title = 'Dengue positive cases'), showlegend = FALSE)
  filename <- paste0("Figures/Case_data/", site, "_denv_v_model.png")
  export(denvPlot, file = filename)

  chikvPlot <- plot_ly() %>%
    add_trace(data=cases.df, x = ~mon.yr, y = ~infected, type = 'scatter', mode='lines', color=I('black'))%>%
    add_trace(data=cases.df, x = ~mon.yr, y = ~chikv, type = 'bar', color=I('purple'), opacity=0.7, yaxis = 'y2')%>%
    layout(margin=list(l=60,r=60,b=100,t=50),
           yaxis = list(side = 'left', title = 'Modeled chikungunya cases', showgrid = FALSE, showline=TRUE), 
           xaxis=list(showline=FALSE, showgrid = FALSE),
           yaxis2 = list(overlaying = "y", side='right', title = 'Chikungunya positive cases'), showlegend = FALSE)
  filename <- paste0("Figures/Case_data/", site, "_chikv_v_model.png")
  export(chikvPlot, file = filename)
  
}


# plot infectious case by day ------------------------------------------------------
infectious <- read.csv("C:/Users/Jamie/Box Sync/FOI Kenya Project/infectious_period.csv", head = T)

# format date
infectious$inf.dates <- as.Date(infectious$inf.dates, "%Y-%m-%d")

# subset observations by disease and site
chik <- infectious[,grepl("dates|CHIKV", names(infectious))]
denv <- infectious[,grepl("dates|DENV", names(infectious))]

chik.chulaimbo <- chik[,grepl("dates|Chulaimbo", names(chik))]
chik.kisumu <- chik[,grepl("dates|Kisumu", names(chik))]
chik.msambweni <- chik[,grepl("dates|Msambweni", names(chik))]
chik.ukunda <- chik[,grepl("dates|Ukunda", names(chik))]
denv.chulaimbo <- denv[,grepl("dates|Chulaimbo", names(denv))]
denv.kisumu <- denv[,grepl("dates|Kisumu", names(denv))]
denv.msambweni <- denv[,grepl("dates|Msambweni", names(denv))]
denv.ukunda <- denv[,grepl("dates|Ukunda", names(denv))]

# sum by date
dfs <- list(chik.chulaimbo, chik.kisumu, chik.msambweni, chik.ukunda, denv.chulaimbo, denv.kisumu, denv.msambweni, denv.ukunda)
Names <- c("Chik.Chulaimbo", "Chik.Kisumu", "Chik.Msambweni", "Chik.Ukunda", "Denv.Chulaimbo", "Denv.Kisumu", "Denv.Msambweni", "Denv.Ukunda")

for (i in 1:length(dfs)){
  diseaseDF <- dfs[[i]]
  # sum positive and negative cases per day
  diseaseDF$neg <- rowSums(diseaseDF[,c(3:(ncol(diseaseDF)-1))]==0, na.rm=T)
  diseaseDF$pos <- rowSums(diseaseDF[,c(3:(ncol(diseaseDF)-1))]==1, na.rm=T)
  # plot cases over time
  diseaseType <- substr(Names[i], 1,4)
  yaxisTitle <- paste0(diseaseType, ' cases')
  site <- substr(Names[i], 6, nchar(Names[i]))
  cases <- plot_ly() %>%
    add_trace(data=diseaseDF, x = ~inf.dates, y = ~pos, type = 'bar', color=I('black'))%>%
    layout(title = site, yaxis = list(title = yaxisTitle, showline=TRUE), xaxis = list(title=""))
  filename <- paste0("Kenya/Figures/Case_data/AIC_Infectious_", diseaseType, "_", site, ".png")
  export(cases, file = filename)
}
