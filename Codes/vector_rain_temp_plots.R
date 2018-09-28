# ------ Make plots overlaying vectors, rainfall, and temperature
rm(list=ls()) #remove previous variable assignments

# --- read in data, if data does not exist, recreate it with code at bottom
vector <- read.csv("C:/Users/Jamie/Box Sync/DENV/Concatenated_Data/ForPlot_VectorDataConcat_MonthYear_2017_06_14.csv", head=T)

plot_ly(vector, x = ~Date, y = ~Ovi_female, type = 'bar', name = 'ovitrap')%>%
  add_trace(y = ~Prok_female, name = 'prokopak') %>%
  add_trace(y = ~HLC_female, name = 'hlc') %>%
  add_trace(y = ~BG_female, name = 'BG') %>%
  layout(yaxis = list(title = 'Female Aedes aegypti mosquito count'), barmode = 'relative',
         margin = list(b = 160), xaxis = list(tickangle = 45)) # barmode='group'

plot_ly(vector, x = ~Date, y = ~Ovi_egg_count, type = 'bar', name = 'ovitrap')%>%
  add_trace(y = ~Prok_total, name = 'prokopak') %>%
  add_trace(y = ~HLC_total, name = 'hlc') %>%
  add_trace(y = ~BG_total, name = 'BG') %>%
  add_trace(y = ~pupae, name = 'pupae') %>%
  add_trace(y = ~larvae, name = 'larvae') %>%
  layout(yaxis = list(title = 'Total Aedes aegypti mosquito count'), barmode = 'relative',
         margin = list(b = 160), xaxis = list(tickangle = 45)) # barmode='group'

#----climate data
Daily_THR_Chulaimbo <- read.csv("Concatenated_Data/climateDataMetrics/GapFilled_Daily_THR_Chulaimbo.csv", head=T)
Daily_THR_Chulaimbo$Date <- as.Date(Daily_THR_Chulaimbo$Date, "%m/%d/%Y")
colnames(Daily_THR_Chulaimbo)[2] <- "Chulaimbo_mean_temp"
colnames(Daily_THR_Chulaimbo)[4] <- "Chulaimbo_mean_rain"

Daily_THR_Kisumu <- read.csv("Concatenated_Data/climateDataMetrics/GapFilled_Daily_THR_Kisumu.csv", head=T)
Daily_THR_Kisumu$Date <- as.Date(Daily_THR_Kisumu$Date, "%m/%d/%Y")
colnames(Daily_THR_Kisumu)[2] <- "Kisumu_mean_temp"
colnames(Daily_THR_Kisumu)[4] <- "Kisumu_mean_rain"

Daily_THR_Msambweni <- read.csv("Concatenated_Data/climateDataMetrics/GapFilled_Daily_THR_Msambweni.csv", head=T)
Daily_THR_Msambweni$Date <- as.Date(Daily_THR_Msambweni$Date, "%Y-%m-%d")
colnames(Daily_THR_Msambweni)[2] <- "Msambweni_mean_temp"
colnames(Daily_THR_Msambweni)[4] <- "Msambweni_mean_rain"

Daily_THR_Ukunda <- read.csv("Concatenated_Data/climateDataMetrics/GapFilled_Daily_THR_Ukunda.csv", head=T)
Daily_THR_Ukunda$Date <- as.Date(Daily_THR_Ukunda$Date, "%Y-%m-%d")
colnames(Daily_THR_Ukunda)[2] <- "Ukunda_mean_temp"
colnames(Daily_THR_Ukunda)[4] <- "Ukunda_mean_rain"

df <- merge(Daily_THR_Chulaimbo, Daily_THR_Kisumu, by="Date", all=T)
df <- merge(df, Daily_THR_Msambweni, by="Date", all=T)
df <- merge(df, Daily_THR_Ukunda, by="Date", all=T)
df <- df[order(df$Date),,drop=FALSE] # reorder dataset by date for plotting

temp <- df
temp$Temp_mean <- apply(temp[, c("Kisumu_mean_temp", "Chulaimbo_mean_temp", "Msambweni_mean_temp", "Ukunda_mean_temp")],1,mean, na.rm=T)
temp$Temp_sd <- apply(temp[, c("Kisumu_mean_temp", "Chulaimbo_mean_temp", "Msambweni_mean_temp", "Ukunda_mean_temp")],1,sd, na.rm=T)
temp$upper <- temp$Temp_mean + temp$Temp_sd
temp$lower <- temp$Temp_mean - temp$Temp_sd
temp <- temp[,c("Date", "Temp_mean", "upper", "lower")]

rain <- df
rain$Rain_mean <- apply(rain[, c("Kisumu_mean_rain", "Chulaimbo_mean_rain", "Msambweni_mean_rain", "Ukunda_mean_rain")],1,mean, na.rm=T)
rain$Rain_sd <- apply(rain[, c("Kisumu_mean_rain", "Chulaimbo_mean_rain", "Msambweni_mean_rain", "Ukunda_mean_rain")],1,sd, na.rm=T)
rain <- rain[,c("Date", "Rain_mean")]

##----------------- female mosquito plot
p1 <- plot_ly() %>% 
  add_trace(data=temp, x = ~Date, y = ~Temp_mean, type = 'scatter', mode = 'lines', name='Mean temperature', yaxis = "y2")%>%
  add_trace(data=temp, x = ~Date, y = ~upper, name = 'Max temp', mode = 'lines+markers', fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'), yaxis = "y2")%>%
  add_trace(data=temp, x = ~Date, y = ~lower, name = 'Min temp', mode = 'lines+markers', fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'), yaxis = "y2")%>%
  add_trace(data=vector, x = ~Date, y = ~Ovi_female, type = 'bar', name = 'Ovitrap')%>%
  add_trace(data=vector, x = ~Date, y = ~Prok_female, name = 'Prokopak') %>%
  add_trace(data=vector, x = ~Date, y = ~HLC_female, name = 'HLC') %>%
  add_trace(data=vector, x = ~Date, y = ~BG_female, name = 'BG') %>%
  layout(title = 'Female Aedes aegypti, temperature, and rainfall in Kenya 2014-2017',
         xaxis = list(title = "Date"), 
         yaxis = list(side = 'left', title = 'Ae. aegypti (count)', showgrid = FALSE, zeroline = TRUE, range=c(0,1000)), barmode='relative',
         yaxis2 = list(side = 'right', overlaying = "y", title = 'Mean temperature (degrees C)', 
                       showgrid = FALSE, zeroline = FALSE, range = c(15, 33)))

p2 <- plot_ly(data=rain, x = ~Date, y = ~Rain_mean,name='Mean rainfall', type = 'scatter', mode = 'lines', color=I('black'))%>%
  layout(yaxis = list(side = 'left', title = 'Mean rainfall (mm)', showgrid = FALSE, zeroline = TRUE))

subplot(p1,p2, nrows = 2, shareX = T, titleX = FALSE, titleY = TRUE) %>% layout(legend = list(orientation = 'h'), margin = (pad=1))

##--------------- total mosquito plot
# p3 <- plot_ly() %>% 
#   add_trace(data=temp, x = ~Date, y = ~Temp_mean, type = 'scatter', mode = 'lines', name='Mean temperature', yaxis = "y2")%>%
#   add_trace(data=temp, x = ~Date, y = ~upper, name = 'Max temp', mode = 'lines+markers', fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'), yaxis = "y2")%>%
#   add_trace(data=temp, x = ~Date, y = ~lower, name = 'Min temp', mode = 'lines+markers', fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'), yaxis = "y2")%>%
#   add_trace(data=vector, x = ~Date, y = ~Ovi_egg_count, type = 'bar', name = 'Ovitrap')%>%
#   add_trace(data=vector, x = ~Date, y = ~Prok_total, name = 'Prokopak') %>%
#   add_trace(data=vector, x = ~Date, y = ~HLC_total, name = 'HLC') %>%
#   add_trace(data=vector, x = ~Date, y = ~BG_total, name = 'BG') %>%
#   add_trace(data=vector, x = ~Date, y = ~larvae, name = 'Larvae') %>%
#   add_trace(data=vector, x = ~Date, y = ~pupae, name = 'Pupae') %>%
#   layout(title = 'Total Aedes aegypti, temperature, and rainfall in Kenya 2014-2017',
#          xaxis = list(title = "Date"),
#          yaxis = list(side = 'left', title = 'Ae. aegypti (count)', showgrid = FALSE, zeroline = TRUE, range=c(0,10000)), barmode='relative',
#          yaxis2 = list(side = 'right', overlaying = "y", title = 'Mean temperature (degrees C)', 
#                        showgrid = FALSE, zeroline = FALSE, range = c(15, 33)))
# 
# p4 <- plot_ly(data=rain, x = ~Date, y = ~Rain_mean,name='Mean rainfall', type = 'scatter', mode = 'lines', color=I('black'))%>%
#   layout(yaxis = list(side = 'left', title = 'Mean rainfall (mm)', showgrid = FALSE, zeroline = TRUE))
# 
# subplot(p3,p4, nrows = 2, shareX = T, titleX = FALSE, titleY = TRUE) %>% layout(legend = list(orientation = 'h'), margin = (pad=1))

#------------------ code to create data for these plots 
library(zoo)
library(plotly)
library(plyr)

files <- list.files("Concatenated_Data/vector/")

for (i in files){
  fileName <- paste0("Concatenated_Data/vector/", i)
  stringLen <- nchar(i) - 4
  tempName <- paste0(substring(i,1,stringLen))
  assign(tempName,read.csv(fileName, header=T))
}

BG <- ddply(BG_aegypti_allSites_2017_05_16, .(Year, Month),
            summarise,
            BG_female = sum(na.omit(Female)),
            BG_total = sum(na.omit(Female)) + sum(na.omit(Male)))  

HLC <- ddply(HLC_aegypti_allSites_2017_05_16, .(Year, Month),
             summarise,
             HLC_female = sum(na.omit(Female)),
             HLC_total = sum(na.omit(Female)) + sum(na.omit(Total_Aedes_aegypti)))  

larvae <- ddply(larvae_aedes_allsites_2017_05_16, .(Year, Month),
                summarise,
                larvae = sum(na.omit(Total_larvae)),
                pupae = sum(na.omit(Pupae)))  

Ovitrap <- ddply(Ovitrap_aegypti_allSites_2017_05_16, .(Year, Month),
                 summarise,
                 Ovi_egg_count = sum(na.omit(Egg_count)),
                 Ovi_female = sum(na.omit(Female)))  

prokopak <- ddply(Prokopak_Aedes_allsites_2017_05_16, .(Year, Month),
                  summarise,
                  Prok_female = sum(Female),
                  Prok_total = sum(na.omit(Female)) + sum(na.omit(Total_mosquitoes)))  

vector <- merge(prokopak, Ovitrap, by=c("Year", "Month"), all=T)
vector <- merge(vector, larvae, by=c("Year", "Month"), all=T)
vector <- merge(vector, HLC, by=c("Year", "Month"), all=T)
vector <- merge(vector, BG, by=c("Year", "Month"), all=T)
vector$Date <- as.yearmon(paste(vector$Year, vector$Month, sep = "-"), "%Y-%B")
vector$Date <- format(vector$Date, "%Y-%m")
write.csv(vector, "C:/Users/Jamie/Box Sync/DENV/Concatenated_Data/ForPlot_VectorDataConcat_MonthYear_2017_06_14.csv", row.names=F)
