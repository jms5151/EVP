# Temperature -----------------------------------------------------------------------------
# Temperature_before_gapfill
plot(wide_data$Date, wide_data$chulaimbo_village_temp_mean_hobo, type='l', xlab='Date', ylab='Mean temperature', main='Chulaimbo village')
plot(wide_data$Date, wide_data$kisumu_estate_temp_mean_hobo, type='l', xlab='Date', ylab='Mean temperature', main='Kisumu estate')
plot(wide_data$Date, wide_data$msambweni_temp_mean_hobo, type='l', xlab='Date', ylab='Mean temperature', main='Msambweni')
plot(wide_data$Date, wide_data$ukunda_temp_mean_hobo, type='l', xlab='Date', ylab='Mean temperature', main='Ukunda')

# Chulaimbo
plot(wide_data$kisumu_mean_temp_gsod, wide_data$chulaimbo_village_temp_mean_hobo, pch=19, cex.lab=1.2, cex.axis=1.2, xlim=c(19,31), ylim=c(19,31), xlab=c("Chulaimbo village HOBO"), ylab=c("GSOD Kisumu Airport"), col=adjustcolor("black", alpha=0.5))
abline(0,1, lty=2)
abline(lm(chulaimbo_village_temp_mean_hobo~kisumu_mean_temp_gsod, data=wide_data), lwd=2)
legend("topleft", legend = c("y=x line", "Regression line"), lty = c(2,1), lwd=c(1,2), bty = "n", cex=1.2)

# Kisumu
plot(wide_data$kisumu_mean_temp_gsod, wide_data$kisumu_estate_temp_mean_hobo, pch=19, cex.lab=1.2, cex.axis=1.2, xlim=c(18,34), ylim=c(18,34), xlab=c("Kisumu estate HOBO"), ylab=c("GSOD Kisumu Airport"), col=adjustcolor("black", alpha=0.5))
abline(0,1, lty=2)
abline(lm(kisumu_estate_temp_mean_hobo~kisumu_mean_temp_gsod, data=wide_data), lwd=2)
legend("topleft", legend = c("y=x line", "Regression line"), lty = c(2,1), lwd=c(1,2), bty = "n", cex=1.2)

# Msambweni
plot(wide_data$mombasa_mean_temp_gsod, wide_data$msambweni_temp_mean_hobo, pch=19, cex.lab=1.2, cex.axis=1.2, xlim=c(23,33), ylim=c(23,33), xlab=c("Msambweni HOBO"), ylab=c("GSOD Mombasa Airport"), col=adjustcolor("black", alpha=0.5))
abline(0,1, lty=2)
abline(lm(msambweni_temp_mean_hobo~mombasa_mean_temp_gsod, data=wide_data), lwd=2)
legend("topleft", legend = c("y=x line", "Regression line"), lty = c(2,1), lwd=c(1,2), bty = "n", cex=1.2)

# Ukunda
plot(wide_data$mombasa_mean_temp_gsod, wide_data$ukunda_temp_mean_hobo, pch=19, cex.lab=1.2, cex.axis=1.2, xlim=c(22,31), ylim=c(22,31), xlab=c("Ukunda HOBO"), ylab=c("GSOD Mombasa Airport"), col=adjustcolor("black", alpha=0.5))
abline(0,1, lty=2)
abline(lm(ukunda_temp_mean_hobo~mombasa_mean_temp_gsod, data=wide_data), lwd=2)
legend("topleft", legend = c("y=x line", "Regression line"), lty = c(2,1), lwd=c(1,2), bty = "n", cex=1.2)

# Temperature_after_gapfill
plot(wide_data$Date, wide_data$chulaimbo_Temperature, type='l', xlab='Date', ylab='Mean temperature', main='Chulaimbo')
plot(wide_data$Date, wide_data$kisumu_Temperature, type='l', xlab='Date', ylab='Mean temperature', main='Kisumu')
plot(wide_data$Date, wide_data$msambweni_Temperature, type='l', xlab='Date', ylab='Mean temperature', main='Msambweni')
plot(wide_data$Date, wide_data$ukunda_Temperature, type='l', xlab='Date', ylab='Mean temperature', main='Ukunda')


# Rainfall --------------------------------------------------------------------------
# Rainfall_before_gapfill
plot(wide_data$Date, wide_data$chulaimbo_village_rainfall_hobo, type='l', xlab='Date', ylab='Rainfall', main='Chulaimbo village')
plot(wide_data$Date, wide_data$obama_rainfall_hobo, type='l', xlab='Date', ylab='Rainfall', main='Kisumu (Obama)')
plot(wide_data$Date, wide_data$msambweni_rainfall_hobo, type='l', xlab='Date', ylab='Rainfall', main='Msambweni')
plot(wide_data$Date, wide_data$ukunda_rainfall_hobo, type='l', xlab='Date', ylab='Rainfall', main='Ukunda')

# Chulaimbo
plot(wide_data$chulaimbo_hospital_daily_rainfall, wide_data$chulaimbo_village_rainfall_hobo, pch=19, cex.lab=1.2, cex.axis=1.2, xlim=c(0,100), ylim=c(0,100), xlab=c("Chulaimbo village HOBO"), ylab=c("ARC"), col=adjustcolor("black", alpha=0.5))
abline(0,1, lty=2)
abline(lm(chulaimbo_village_rainfall_hobo~chulaimbo_hospital_daily_rainfall, data=wide_data), lwd=2)
legend("topleft", legend = c("y=x line", "Regression line"), lty = c(2,1), lwd=c(1,2), bty = "n", cex=1.2)

# Kisumu
plot(wide_data$obama_daily_rainfall, wide_data$obama_rainfall_hobo, pch=19, cex.lab=1.2, cex.axis=1.2, xlim=c(0,80), ylim=c(0,80), xlab=c("Kisumu (Obama) HOBO"), ylab=c("ARC"), col=adjustcolor("black", alpha=0.5))
abline(0,1, lty=2)
abline(lm(obama_rainfall_hobo~obama_daily_rainfall, data=wide_data), lwd=2)
legend("topleft", legend = c("y=x line", "Regression line"), lty = c(2,1), lwd=c(1,2), bty = "n", cex=1.2)

# Msambweni
plot(wide_data$msambweni_daily_rainfall, wide_data$msambweni_rainfall_hobo, pch=19, cex.lab=1.2, cex.axis=1.2, xlim=c(0,250), ylim=c(0,250), xlab=c("Msambweni HOBO"), ylab=c("ARC"), col=adjustcolor("black", alpha=0.5))
abline(0,1, lty=2)
abline(lm(msambweni_rainfall_hobo~msambweni_daily_rainfall, data=wide_data), lwd=2)
legend("topleft", legend = c("y=x line", "Regression line"), lty = c(2,1), lwd=c(1,2), bty = "n", cex=1.2)

# Ukunda
plot(wide_data$ukunda_daily_rainfall, wide_data$ukunda_rainfall_hobo, pch=19, cex.lab=1.2, cex.axis=1.2, xlim=c(0,250), ylim=c(0,250), xlab=c("Ukunda HOBO"), ylab=c("ARC"), col=adjustcolor("black", alpha=0.5))
abline(0,1, lty=2)
abline(lm(ukunda_rainfall_hobo~ukunda_daily_rainfall, data=wide_data), lwd=2)
legend("topleft", legend = c("y=x line", "Regression line"), lty = c(2,1), lwd=c(1,2), bty = "n", cex=1.2)

# Rainfall_after_gapfill
plot(wide_data$Date, wide_data$chulaimbo_Rainfall, type='l', xlab='Date', ylab='Rainfall (mm)', main='Chulaimbo')
plot(wide_data$Date, wide_data$kisumu_Rainfall, type='l', xlab='Date', ylab='Rainfall (mm)', main='Kisumu')
plot(wide_data$Date, wide_data$msambweni_Rainfall, type='l', xlab='Date', ylab='Rainfall (mm)', main='Msambweni')
plot(wide_data$Date, wide_data$ukunda_Rainfall, type='l', xlab='Date', ylab='Rainfall (mm)', main='Ukunda')
