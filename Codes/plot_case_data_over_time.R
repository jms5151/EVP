# plot AIC disease cases over time --------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load data
load("Concatenated_Data/case_data/merged_case_data.RData")

# plot lab-confirmed data -----------------------------------------------------
sitenames <- unique(cases$Site)

for (i in 1:length(sitenames)){
  x<-subset(cases, Site == sitenames[i] & Date > '2014-01-01')
  tiff(paste0("Figures/case_data/dengue_cases_", sitenames[i], ".tiff"))
  plot(x$Date, x$denv_positive, type='b', pch=16, cex.axis=1.3, cex.lab=1.3, cex=1.3, lwd=2, xlab='Date', ylab='Dengue cases', main=paste0('Dengue ', sitenames[i]), las=1)
  dev.off()
  if (sum(x$chikv_positive, na.rm=T)>1){
    tiff(paste0("Figures/case_data/chikungunya_cases_", sitenames[i], ".tiff"))
    plot(x$Date, x$chikv_positive, type='b', pch=16, cex.axis=1.3, cex.lab=1.3, cex=1.3, lwd=2, xlab='Date', ylab='Chikungunya cases', main=paste0('Chikungunya ', sitenames[i]), las=1)
    dev.off()
  }
}

# plot clinically suspected data --------------------------------------------
sitenames2 <- unique(cases$Site[cases$country=="Ecuador"])

for (i in 1:length(sitenames2)){
  x<-subset(cases, Site == sitenames2[i])
  tiff(paste0("Figures/case_data/suspected_dengue_cases_", sitenames2[i], ".tiff"))
  plot(x$Date, x$denv_positive_clinically_diagnosed, type='b', pch=16, cex.axis=1.3, cex.lab=1.3, cex=1.3, lwd=2, xlab='Date', ylab='Dengue cases', main=paste0('Suspected dengue ', sitenames2[i]), las=1)
  dev.off()
  if (sum(x$chikv_positive_clinically_diagnosed, na.rm=T)>1){
    x<-subset(x, Date > '2015-01-01')
    tiff(paste0("Figures/case_data/suspected_chikungunya_cases_", sitenames[i], ".tiff"))
    plot(x$Date, x$chikv_positive_clinically_diagnosed, type='b', pch=16, cex.axis=1.3, cex.lab=1.3, cex=1.3, lwd=2, xlab='Date', ylab='Chikungunya cases', main=paste0('Suspected chikungunya ', sitenames2[i]), las=1)
    dev.off()
  }
}