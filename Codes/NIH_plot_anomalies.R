# import satellite-derived climate data --------------------------------
source("Codes/REDCap_extract_climate_and_vector_data.R")
library(plyr)

# calculate daily temperature and rainfall anomalies -------------------
# subset satellite data for Ukunda and Kisumu sites
sat.data <- redcap_clim_vec[, grepl("date_collected$|^redcap_event_name$|lst|^daily_rainfall", names(redcap_clim_vec))]
sat.data <- subset(sat.data, redcap_event_name=="obama_arm_1"|redcap_event_name=="ukunda_arm_1")

# replace very high ltm rainfall values with NA
sat.data$daily_rainfall_long_term_mean[sat.data$daily_rainfall_long_term_mean > 50] <- NA

# average across chulaimbo and kisumu loggers
sat.data.anomalies <- ddply(sat.data, .(date_collected, redcap_event_name)
                       , summarize
                       , temp_anomaly = mean_lst - ltm_lst
                       , rain_anomaly = daily_rainfall - daily_rainfall_long_term_mean)


# format dates for plotting
sat.data.anomalies$date_collected <- as.Date(sat.data.anomalies$date_collected, "%Y-%m-%d")
sat.data.anomalies <- subset(sat.data.anomalies, date_collected > "2014-06-01" & date_collected < "2018-05-01")
sat.data.anomalies$mon.yr <- format(sat.data.anomalies$date_collected, "%Y-%m")

# separate data by site
kisumu.clim <- subset(sat.data.anomalies, redcap_event_name == "obama_arm_1")
ukunda.clim <- subset(sat.data.anomalies, redcap_event_name == "ukunda_arm_1")

# plot temperature anomalies -------------------------------------------------------------
plot(kisumu.clim$date_collected, kisumu.clim$temp_anomaly, type='h', ylab=c("Temperature anomaly (degrees C)"), xlab="")
plot(ukunda.clim$date_collected, ukunda.clim$temp_anomaly, type='h', ylab=c("Temperature anomaly (degrees C)"), xlab="")

# plot rainfall anomalies -------------------------------------------------------------
plot(kisumu.clim$date_collected, kisumu.clim$rain_anomaly, type='h', ylab=c("Rainfall anomaly (mm)"), xlab="")
plot(ukunda.clim$date_collected, ukunda.clim$rain_anomaly, type='h', ylab=c("Rainfall anomaly (mm)"), xlab="")

# load case data -----------------------------------------------------------
aic_cases <- read.csv("Kenya/Concatenated_Data/aic_timeline.csv", head=T, stringsAsFactors = F)
aic_cases[,c("visit_a_int_date", "infected.t0", "infected.t1")] <- lapply(aic_cases[,c("visit_a_int_date", "infected.t0", "infected.t1")], as.Date)
aic_cases$mon.yr <- format(aic_cases$infected.t0, "%Y-%m")

# Kisumu
kisumu_aic_cases <- subset(aic_cases, id_site == "Kisumu")
kisumu_aic.df <- ddply(kisumu_aic_cases, .(mon.yr)
                , summarise
                , denv = sum(visit_a_infected_denv == 1, na.rm=T)
                , chikv = sum(visit_a_infected_chikv == 1, na.rm=T))

# Ukunda
ukunda_aic_cases <- subset(aic_cases, id_site == "Ukunda")
ukunda_aic.df <- ddply(ukunda_aic_cases, .(mon.yr)
                       , summarise
                       , denv = sum(visit_a_infected_denv == 1, na.rm=T)
                       , chikv = sum(visit_a_infected_chikv == 1, na.rm=T))

# plot ------------------------------------------------------------------------------
library(plotly)

plot_ly(data=kisumu_aic.df, x = ~mon.yr, y = ~denv, color=I('black')) %>%
          layout(margin=list(l=60,r=60,b=100,t=50),
                 yaxis = list(side = 'left', title = 'Dengue cases', showgrid = FALSE, showline=TRUE),
                 xaxis=list(showline=FALSE, showgrid = FALSE, title = ''))

plot_ly(data=kisumu_aic.df, x = ~mon.yr, y = ~chikv, color=I('black')) %>%
  layout(margin=list(l=60,r=60,b=100,t=50),
         yaxis = list(side = 'left', title = 'Chikungunya cases', showgrid = FALSE, showline=TRUE),
         xaxis=list(showline=FALSE, showgrid = FALSE, title = ''))

plot_ly(data=ukunda_aic.df, x = ~mon.yr, y = ~denv, color=I('black')) %>%
  layout(margin=list(l=60,r=60,b=100,t=50),
         yaxis = list(side = 'left', title = 'Dengue cases', showgrid = FALSE, showline=TRUE),
         xaxis=list(showline=FALSE, showgrid = FALSE, title = ''))

plot_ly(data=ukunda_aic.df, x = ~mon.yr, y = ~chikv, color=I('black')) %>%
  layout(margin=list(l=60,r=60,b=100,t=50),
         yaxis = list(side = 'left', title = 'Chikungunya cases', showgrid = FALSE, showline=TRUE),
         xaxis=list(showline=FALSE, showgrid = FALSE, title = ''))
