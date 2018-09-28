#--- vector v rainfall

#--- read in vector data and add prefixes to column names
# hlc <- read.csv("Concatenated_Data/vector/hlc.csv", head=T, as.is=T)
# larvae <- read.csv("Concatenated_Data/vector/larvae.csv", head=T, as.is=T)
# prokopak <- read.csv("Concatenated_Data/vector/prokopak.csv", head=T, as.is=T)

#--- climate data
climate.all.sites <- read.csv("C:/Users/Jamie/Box Sync/DENV/Concatenated_Data/climate/climate_all_sites.csv", head=T)
climate.all.sites$Date <- as.Date(climate.all.sites$Date, "%Y-%m-%d")

ch.hosp.climate <- subset(climate.all.sites, site == "chulaimbo_hospital")
ms.climate <- subset(climate.all.sites, site == "msambweni")
ki.hosp.climate <- subset(climate.all.sites, site == "obama")
uk.climate <- subset(climate.all.sites, site == "ukunda")

#--- BG data
bg <- read.csv("Concatenated_Data/vector/bg.csv", head=T, as.is=T)
bg$Date <- as.Date(bg$pickupDate, "%Y-%m-%d")
bg.df <- bg[, c("Date", "study_site", "a.aegypti_total")]
bg.df$cumRain_1month <- as.numeric(NA)
bg.df$cumRain_2months <- as.numeric(NA)

for (i in 1:nrow(bg.df)){
  # determine date mosquito trap collected
  vecdate <- bg.df$Date[i]
  # subset climate data by study site
  if (bg.df$study_site[i] == "Kisumu"){
    clim <- ki.hosp.climate
  } else if (bg.df$study_site[i] == "Chulaimbo"){
    clim <- ch.hosp.climate
  } else if (bg.df$study_site[i] == "Ukunda"){
    clim <- uk.climate
  } else if (bg.df$study_site[i] == "Msambweni"){
    clim <- ms.climate
  }
  # check if date exists in climate dataset and enter NA if not
  if (vecdate %in% clim[,"Date"]==TRUE){
    # calculate cumulative rain in month prior
    LastRow <- which(clim[,"Date"] == vecdate)
    priorMonth <- vecdate-30
    FirstRow30 <- which(clim[,"Date"] == priorMonth)
    if (length(FirstRow30)!=0){
      vector30 <- clim[c(FirstRow30:LastRow),"meanRain"]
      if (all(is.na(vector30))){
        bg.df$cumRain_1month[i] <- NA
      } else {
        bg.df$cumRain_1month[i] <- sum(clim[c(FirstRow30:LastRow),"meanRain"], na.rm = T)
      }
      # calculate cumulative rain in two months prior
      prior2Months <- vecdate-60
      FirstRow60 <- which(clim[,"Date"] == prior2Months)
      if (length(FirstRow60)!=0){
        vector60 <- clim[c(FirstRow60:LastRow),"meanRain"]
        if (all(is.na(vector60))){
          bg.df$cumRain_2months[i] <- NA
        } else {
        bg.df$cumRain_2months[i] <- sum(clim[c(FirstRow60:LastRow),"meanRain"], na.rm = T)
        }
      } 
    }
  }
}

#--- determine if survey was conducted after BG data collection methods changed in May 2015
for (i in 1:nrow(bg.df)){
  bg.df$trapPeriod[i] <- ifelse(bg.df$Date[i]<"2015-05-01", 1, 2)
}

#-- make plots
tiff("Figures/vectors/vectors_v_rainfall/BG_Mosquitoes_v_cumRain_1month_bySite.tiff", width=751, height=501)
plot(bg.df$cumRain_1month, bg.df$a.aegypti_total, pch=21, 
     col = c("black", "black", "black", "black")[as.numeric(bg.df$study_site)], 
     bg=c("blue", "yellow", "red", "green"), cex=1.5, xlab=c("Cumulative rain (1 month prior to survey)"),
     ylab=c("Mosquitoes (BG trap count)"))
legend("topleft", inset=.02, title="Time period",
       c("Kisumu","Chulaimbo", "Ukunda", "Msambweni"), fill=c("blue", "yellow", "red", "green"), cex=0.8)
dev.off()

tiff("Figures/vectors/vectors_v_rainfall/BG_Mosquitoes_v_cumRain_1month.tiff", width=751, height=501)
plot(bg.df$cumRain_1month, bg.df$a.aegypti_total, pch=21, 
     col = c("black", "black")[as.numeric(bg.df$trapPeriod)], 
     bg=c("blue", "yellow"), cex=1.5, xlab=c("Cumulative rain (1 month prior to survey)"),
     ylab=c("Mosquitoes (BG trap count)"))
legend("topleft", inset=.02, title="Time period",
       c("Before May 2015","After May 2015"), fill=c("blue", "yellow"), horiz=TRUE, cex=0.8)
dev.off()

tiff("Figures/vectors/vectors_v_rainfall/BG_Mosquitoes_v_cumRain_2months.tiff", width=751, height=501)
plot(bg.df$cumRain_2months, bg.df$a.aegypti_total, pch=21, 
     col = c("black", "black")[as.numeric(bg.df$trapPeriod)], 
     bg=c("blue", "yellow"), cex=1.5, xlab=c("Cumulative rain (2 months prior to survey)"),
     ylab=c("Mosquitoes (BG trap count)"))
legend("topleft", inset=.02, title="Time period",
       c("Before May 2015","After May 2015"), fill=c("blue", "yellow"), horiz=TRUE, cex=0.8)
dev.off()

#---- ovitraps
ovitrap <- read.csv("Concatenated_Data/vector/ovitrap.csv", head=T, as.is=T)
ovitrap$Date <- as.Date(ovitrap$date_collected, "%Y-%m-%d")
ovi.df <- ovitrap[, c("Date", "study_site", "eggs")]
ovi.df$cumRain_1month <- as.numeric(NA)
ovi.df$cumRain_2months <- as.numeric(NA)

for (i in 1:nrow(ovi.df)){
  # determine date mosquito trap collected
  vecdate <- ovi.df$Date[i]
  # subset climate data by study site
  if (ovi.df$study_site[i] == "Kisumu"){
    clim <- ki.hosp.climate
  } else if (ovi.df$study_site[i] == "Chulaimbo"){
    clim <- ch.hosp.climate
  } else if (ovi.df$study_site[i] == "Ukunda"){
    clim <- uk.climate
  } else if (ovi.df$study_site[i] == "Msambweni"){
    clim <- ms.climate
  }
  # check if date exists in climate dataset and enter NA if not
  if (vecdate %in% clim[,"Date"]==TRUE){
    # calculate cumulative rain in month prior
    LastRow <- which(clim[,"Date"] == vecdate)
    priorMonth <- vecdate-30
    FirstRow30 <- which(clim[,"Date"] == priorMonth)
    if (length(FirstRow30)!=0){
      vector30 <- clim[c(FirstRow30:LastRow),"meanRain"]
      if (all(is.na(vector30))){
        ovi.df$cumRain_1month[i] <- NA
      } else {
        ovi.df$cumRain_1month[i] <- sum(clim[c(FirstRow30:LastRow),"meanRain"], na.rm = T)
      }
      # calculate cumulative rain in two months prior
      prior2Months <- vecdate-60
      FirstRow60 <- which(clim[,"Date"] == prior2Months)
      if (length(FirstRow60)!=0){
        vector60 <- clim[c(FirstRow60:LastRow),"meanRain"]
        if (all(is.na(vector60))){
          ovi.df$cumRain_2months[i] <- NA
        } else {
          ovi.df$cumRain_2months[i] <- sum(clim[c(FirstRow60:LastRow),"meanRain"], na.rm = T)
        }
      } 
    }
  }
}

#-- make plots
tiff("Figures/vectors/vectors_v_rainfall/Ovitrap_mosquitoes_v_cumRain_1month_bySite.tiff", width=751, height=501)
plot(ovi.df$cumRain_1month, ovi.df$a.aegypti_total, pch=16, 
     xlab=c("Cumulative rain (1 month prior to survey)"),
     ylab=c("Mosquitoes (Ovitrap count)"))
dev.off()

tiff("Figures/vectors/vectors_v_rainfall/Ovitrap_mosquitoes_v_cumRain_1month.tiff", width=751, height=501)
plot(ovi.df$cumRain_1month, ovi.df$a.aegypti_total, pch=16, 
     xlab=c("Cumulative rain (1 month prior to survey)"),
     ylab=c("Mosquitoes (Ovitrap count)"))
dev.off()

tiff("Figures/vectors/vectors_v_rainfall/Ovitrap_mosquitoes_v_cumRain_2months.tiff", width=751, height=501)
plot(ovi.df$cumRain_2months, ovi.df$a.aegypti_total, pch=16, 
     xlab=c("Cumulative rain (2 months prior to survey)"),
     ylab=c("Mosquitoes (Ovitrap count)"))
dev.off()

