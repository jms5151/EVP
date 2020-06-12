# visualize intervention results ------------------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load library
library(ggplot2)

# boxplot of all results --------------------------------------------------------------
load("Concatenated_Data/all_interventions.R")

# plot
# save as pdf width=7.19, height=4.82
ggplot(all_interventions, aes(x=Intervention_strategy, y=meanPercReduct)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  facet_wrap(~Intervention_name) +
  geom_errorbar(aes(ymin=meanPercReduct-sePercReduct, ymax=meanPercReduct+sePercReduct), width=.2,
                position=position_dodge(.9)) +
  labs(y = "Disease cases averted (%)")+
  scale_x_continuous("Intervention intensity (% reduction)", breaks=c(0.1, 0.5, 0.9), labels=c(10, 50, 90)) +
  theme_classic() +
  theme(text = element_text(size=12))


# plot of spray by date --------------------------------------------------------------
# load data
spray_intervention <- read.csv("Concatenated_Data/model_simulations/SEI-SEIR_simulations_with_intervention_spray.csv", head=T, stringsAsFactors=F)

# visualize data
sites <- unique(spray_intervention$Site)
dates <- unique(spray_intervention$Intervention_date)
dir.create("Figures/Intervention_plots/")

for (i in 1:length(sites)){
  x <- subset(spray_intervention, Site == sites[i] & Intervention_date == "None" & time > 90)
  for (j in 2:length(dates)){
    tiffName <- paste0("Figures/Intervention_plots/Intervention_", sites[i], "_", dates[j], ".tiff")
    tiff(tiffName, width = 1052, height = 603, pointsize = 16)
    plot(x$time, x$I, type='l', lwd=2, cex.axis=1.3, cex.lab=1.3, ylab="Number infected", xlab="Time") #, ylim=c(ymin, ymax)
    x2 <- subset(spray_intervention, Site == sites[i] & Intervention_date == dates[j])
    lines(x2$time[x2$Intervention_strategy==0.1], x2$I[x2$Intervention_strategy==0.1], type='l', lwd=2, col='lightblue', xaxt="n", yaxt="n")
    lines(x2$time[x2$Intervention_strategy==0.5], x2$I[x2$Intervention_strategy==0.5], type='l', lwd=3, col='blue', xaxt="n", yaxt="n")
    lines(x2$time[x2$Intervention_strategy==0.9], x2$I[x2$Intervention_strategy==0.9], type='l', lwd=3, col='darkblue', xaxt="n", yaxt="n")
    title(paste0(sites[i], '\n', "Intervention date: ", dates[j]), cex=1.3)
    legend("topleft", legend = c("No reduction", "10% reduction", "50% reduction", "90% reduction"),
           lty = c(1,1,1,1), lwd = c(2,2,2,2),
           col = c("black", "lightblue", "blue", "darkblue"), 
           text.col = c("black", "lightblue", "blue", "darkblue"),
           bty = "n", cex=1.3)
    dev.off()
  }
}