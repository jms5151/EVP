# plot pre-gapfilled weather data for supplemental materials ---------------------------------------
rm(list=ls()) #remove previous variable assignments

# load
wide_data_ecuador <- read.csv("Concatenated_Data/climate_data/pregapfilled_data_Ecuador.csv", head=T, stringsAsFactors = F)
wide_data_kenya <- read.csv("Concatenated_Data/climate_data/pregapfilled_data_Kenya.csv", head=T, stringsAsFactors = F)

# Temperature -------------------------------------------------------------------------
par(mfrow=c(3, 2))

# Huaquillas & Machala
plot(wide_data_ecuador$Machala_temp, wide_data_ecuador$Huaquillas_temp, pch=19, cex.lab=1.2, cex.axis=1.2, xlim=c(20,32), ylim=c(20,32), xlab=c("Machala temp."), ylab=c("Huaquillas temp."), col=adjustcolor("black", alpha=0.5))
abline(0,1, lty=2)
abline(lm(Huaquillas_temp~Machala_temp, data=wide_data_ecuador), lwd=2)
# legend("topleft", legend = c("y=x line", "Regression line"), lty = c(2,1), lwd=c(1,2), bty = "n", cex=1.2)

# Portovelo & Zaruma
plot(wide_data_ecuador$Portovelo_temp, wide_data_ecuador$Zaruma_temp, pch=19, cex.lab=1.2, cex.axis=1.2, xlim=c(19,28), ylim=c(19,28), xlab=c("Portovelo temp."), ylab=c("Zaruma temp."), col=adjustcolor("black", alpha=0.5))
abline(0,1, lty=2)
abline(lm(Zaruma_temp~Portovelo_temp, data=wide_data_ecuador), lwd=2)

# Chulaimbo
plot(wide_data_kenya$kisumu_mean_temp_gsod, wide_data_kenya$chulaimbo_village_temp_mean_hobo, pch=19, cex.lab=1.2, cex.axis=1.2, xlim=c(19,31), ylim=c(19,31), xlab=c("Chulaimbo temp."), ylab=c("GSOD temp."), col=adjustcolor("black", alpha=0.5))
abline(0,1, lty=2)
abline(lm(chulaimbo_village_temp_mean_hobo~kisumu_mean_temp_gsod, data=wide_data_kenya), lwd=2)

# Kisumu
plot(wide_data_kenya$kisumu_mean_temp_gsod, wide_data_kenya$kisumu_estate_temp_mean_hobo, pch=19, cex.lab=1.2, cex.axis=1.2, xlim=c(18,34), ylim=c(18,34), xlab=c("Kisumu  temp."), ylab=c("GSOD temp."), col=adjustcolor("black", alpha=0.5))
abline(0,1, lty=2)
abline(lm(kisumu_estate_temp_mean_hobo~kisumu_mean_temp_gsod, data=wide_data_kenya), lwd=2)

# Msambweni
plot(wide_data_kenya$mombasa_mean_temp_gsod, wide_data_kenya$msambweni_temp_mean_hobo, pch=19, cex.lab=1.2, cex.axis=1.2, xlim=c(23,33), ylim=c(23,33), xlab=c("Msambweni temp."), ylab=c("GSOD temp."), col=adjustcolor("black", alpha=0.5))
abline(0,1, lty=2)
abline(lm(msambweni_temp_mean_hobo~mombasa_mean_temp_gsod, data=wide_data_kenya), lwd=2)

# Ukunda
plot(wide_data_kenya$mombasa_mean_temp_gsod, wide_data_kenya$ukunda_temp_mean_hobo, pch=19, cex.lab=1.2, cex.axis=1.2, xlim=c(22,31), ylim=c(22,31), xlab=c("Ukunda temp."), ylab=c("GSOD temp."), col=adjustcolor("black", alpha=0.5))
abline(0,1, lty=2)
abline(lm(ukunda_temp_mean_hobo~mombasa_mean_temp_gsod, data=wide_data_kenya), lwd=2)

# 900 x 950

# Rainfall -------------------------------------------------------------------------
par(mfrow=c(3, 2))

# Huaquillas & Machala
plot(wide_data_ecuador$Machala_Two_week_rainfall, wide_data_ecuador$Huaquillas_Two_week_rainfall, pch=19, cex.lab=1.2, cex.axis=1.2, xlim=c(0,400), ylim=c(0,400), xlab=c("Machala rainfall"), ylab=c("Huaquillas rainfall"), col=adjustcolor("black", alpha=0.5))
abline(0,1, lty=2)
abline(lm(Huaquillas_Two_week_rainfall~Machala_Two_week_rainfall, data=wide_data_ecuador), lwd=2)
# legend("topleft", legend = c("y=x line", "Regression line"), lty = c(2,1), lwd=c(1,2), bty = "n", cex=1.2)

# Portovelo & Zaruma
plot(wide_data_ecuador$Portovelo_Two_week_rainfall, wide_data_ecuador$Zaruma_Two_week_rainfall, pch=19, cex.lab=1.2, cex.axis=1.2, xlim=c(0,250), ylim=c(0,250), xlab=c("Portovelo rainfall"), ylab=c("Zaruma rainfall"), col=adjustcolor("black", alpha=0.5))
abline(0,1, lty=2)
abline(lm(Zaruma_Two_week_rainfall~Portovelo_Two_week_rainfall, data=wide_data_ecuador), lwd=2)

# Chulaimbo
plot(wide_data_kenya$Two_week_rainfall_chulaimbo, wide_data_kenya$Two_week_rainfall_chulaimbo_noaa, pch=19, cex.lab=1.2, cex.axis=1.2, xlim=c(0,300), ylim=c(0,300), xlab=c("Chulaimbo rainfall"), ylab=c("ARC2 rainfall"), col=adjustcolor("black", alpha=0.5))
abline(0,1, lty=2)
abline(lm(Two_week_rainfall_chulaimbo_noaa~Two_week_rainfall_chulaimbo, data=wide_data_kenya), lwd=2)

# Kisumu
plot(wide_data_kenya$Two_week_rainfall_obama, wide_data_kenya$Two_week_rainfall_obama_noaa, pch=19, cex.lab=1.2, cex.axis=1.2, xlim=c(0,300), ylim=c(0,300), xlab=c("Kisumu rainfall"), ylab=c("ARC2 rainfall"), col=adjustcolor("black", alpha=0.5))
abline(0,1, lty=2)
abline(lm(Two_week_rainfall_obama_noaa~Two_week_rainfall_obama, data=wide_data_kenya), lwd=2)

# Msambweni
plot(wide_data_kenya$Two_week_rainfall_msambweni, wide_data_kenya$Two_week_rainfall_msambweni_noaa, pch=19, cex.lab=1.2, cex.axis=1.2, xlim=c(0,400), ylim=c(0,400), xlab=c("Msambweni rainfall"), ylab=c("ARC2 rainfall"), col=adjustcolor("black", alpha=0.5))
abline(0,1, lty=2)
abline(lm(Two_week_rainfall_msambweni_noaa~Two_week_rainfall_msambweni, data=wide_data_kenya), lwd=2)

# Ukunda
plot(wide_data_kenya$Two_week_rainfall_ukunda, wide_data_kenya$Two_week_rainfall_ukunda_noaa, pch=19, cex.lab=1.2, cex.axis=1.2, xlim=c(0,400), ylim=c(0,400), xlab=c("Ukunda rainfall"), ylab=c("ARC2 rainfall"), col=adjustcolor("black", alpha=0.5))
abline(0,1, lty=2)
abline(lm(Two_week_rainfall_ukunda_noaa~Two_week_rainfall_ukunda, data=wide_data_kenya), lwd=2)
