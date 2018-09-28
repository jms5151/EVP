#------ Create adjusted weather underground data and import to REDCap
rm(list=ls()) #remove previous variable assignments

#-- bring in data
source("/Codes/REDCap_extractClimateData.R") # import hobo logger climate data from REDCap
wu_coast <- read.csv("Climate/wu_coast.csv", head=T)
wu_coast$Date <- as.Date(wu_coast$Date, "%m/%d/%y")
wu_west <- read.csv("Climate/wu_west.csv", head=T)
wu_west$Date <- as.Date(wu_west$Date, "%m/%d/%Y")

#---find out what dates are missing
hobo <- kisumu[!is.na(kisumu$temp_mean_hobo),]
hobo <- hobo$Date
wu <-  seq.Date(from=min(hobo), to=max(hobo), by='1 day')
x<-wu[!wu %in% hobo]

y<-merge(chulaimbo, kisumu, by="Date")
plot(y$rh_mean_hobo.x, y$rh_mean_hobo.y)
abline(lm(y$rh_mean_hobo.y~y$rh_mean_hobo.x))
cor.test(y$rh_mean_hobo.x, y$rh_mean_hobo.y)
lm(y$rh_mean_hobo.y~y$rh_mean_hobo.x)

z<-merge(ukunda, wu_coast, by="Date")
plot(z$temp_mean_hobo, z$Max_TemperatureC)
cor.test(z$temp_mean_hobo, z$Max_TemperatureC)
lm(z$temp_mean_hobo ~ z$Max_TemperatureC)

#------ Compare temperature
logger <- list(chulaimbo, kisumu, msambweni, ukunda)
hoboVariable <- c("temp_mean_hobo", "rh_mean_hobo", "rainfall_mean_hobo")

for (i in 1:4){
  hobo <- as.data.frame(logger[i])
  if (i == 1){
    city <- "Chulaimbo"
    wu <- wu_west
  } else if (i == 2) {
    city <- "Kisumu"
    wu <- wu_west
  } else if (i == 3){
    city <- "Msambweni"
    wu <- wu_coast
  } else if (i == 4){
    city <- "Ukunda"
    wu <- wu_coast
  }
  df2 <-  merge(hobo, wu, by="Date", all.y=T)
  for (hoboVar in hoboVariable){
    if (hoboVar == "Temp_mean"){
      wuVar <- "Mean_TemperatureC"
    } else if (hoboVar == "Humidity_mean"){
      wuVar <- "Mean_Humidity"
    } else if (hoboVar == "Rain_mean") {
      wuVar <- "PrecipitationMM"
    }
    df <- merge(hobo[,c("Date", hoboVar)], wu[,c("Date", wuVar)], by="Date")
    df <- df[complete.cases(df),]
    # plot
    filename <-  paste0("C:/Users/Jamie/Box Sync/DENV/Figures/climate_data_comparisons/", city, "_WU_", wuVar, "_v_HOBO_", hoboVar, ".tiff")
    tiff(file=filename,  height = 544, width = 900)
    plot(df[,wuVar], df[,hoboVar], xlab=c("WU ", wuVar), ylab=c("HOBO ", hoboVar))
    abline(lm(df[,hoboVar] ~ df[,wuVar]))
    lm = lm(df[,hoboVar] ~ df[,wuVar])
    coefs <- coef(lm)
    b0 <- (coefs[[1]])
    b1 <- (coefs[[2]])
    r2 <- (summary(lm)$r.squared)
    eqn <- bquote(italic(y) == .(b0) + .(b1)*italic(x) * "," ~~ r^2 == .(r2))
    mtext(eqn)
    dev.off()
    # fill in missing data
    if (hoboVar == "Temp_mean" | hoboVar == "Humidity_mean"){
      rowsOrig <- length(df2[,hoboVar])
      rowsComplete <- length(na.omit(df2[,hoboVar]))
      if (rowsOrig > rowsComplete){
        for (i in 1:rowsOrig){
          if(is.na(df2[i,hoboVar]) == TRUE){
            hoboDate <- df2$Date[i]
            WUrow <- which(wu$Date == hoboDate)
            wuData <- wu[WUrow, wuVar]
            if (length(wuData) == 0){
              df2[i,hoboVar] <- NA
            } else {
              df2[i,hoboVar] <- b0 + b1 * wuData  
            }
          }
        }
      }
      
      filename2 <- paste0("C:/Users/Jamie/Box Sync/DENV/Concatenated_Data/climateDataMetrics/GapFilled_Daily_THR_", city, ".csv")
      newDF <- df2[, c("Date", "Temp_mean", "Humidity_mean", "Rain_mean")]
      write.csv(newDF, filename2, row.names=F)        
    }
  }
}


