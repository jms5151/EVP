# plot AIC disease cases over time --------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load library
library(plotly)
library(ggplot2)
library(plyr)
library(webshot)

# Kenya cases ----------------------------------------------------------------
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

# Ecuador cases --------------------------------------------------------------
cases0311 <- read.csv("Concatenated_Data/case_data/cases_by_week_Ecuador_2003_2011.csv", head=T, stringsAsFactors = F)
cases1718 <- read.csv("Concatenated_Data/case_data/cases_by_week_Ecuador_2017_2018.csv", head=T, stringsAsFactors = F)
sites <- unique(cases0311$Site)
casesList<-list(cases0311, cases1718)
years<-c("2003-2011", "2017-2018")

for (x in 1:length(casesList)){
  cases <- casesList[[x]]
  for (i in 1:length(sites)){
    diseaseDF <- subset(cases, Site == sites[i])
    caseplots <- plot_ly() %>%
      add_trace(data=diseaseDF, x = ~Year.Month, y = ~denv_positive, type = 'bar', color=I('black'))%>%
      layout(margin=list(l=60,r=60,b=100,t=50), title = sites[i], yaxis = list(title = "Dengue cases", showline=TRUE), xaxis = list(title=""))
    filename <- paste0("Ecuador/Figures/Dengue_cases_", sites[i], "_", years[x], ".png")
    export(caseplots, file = filename)
  }
}