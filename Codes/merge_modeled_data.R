# merge modeled data -----------------------------------------
rm(list=ls()) #remove previous variable assignments

library(plyr)

# load data and add cumulative mosquitoes column
files <- list.files("Concatenated_Data/model_simulations/")
labels <- c("H", "R", "T", "THR", "THRvar")

for (i in 1:length(files)){
  fileName <- paste0("Concatenated_Data/model_simulations/", files[i])
  x <- read.csv(fileName)
  newNameM <- paste0("Mtot_", labels[i])
  x[newNameM] <- x$M1 + x$M2 + x$M3
  newNameI <- paste0("I_", labels[i])
  x[newNameI] <- x$I
  x <- subset(x, time >= 100)
  assign(labels[i], x)
}

# format model with trait variation
traitvar <- ddply(THRvar, .(Date, Site)
                  , summarize
                  , mean.I = mean(na.omit(I_THRvar))
                  , min.I = mean.I  - sd(na.omit(I_THRvar))
                  , max.I = mean.I + sd(na.omit(I_THRvar))
                  , mean.M = mean(na.omit(Mtot_THRvar))
                  , min.M = mean.M - sd(na.omit(Mtot_THRvar))
                  , max.M = mean.M + sd(na.omit(Mtot_THRvar)))

# traitvar$min.I[traitvar$min.I < 0] <- 0
traitvar$min.I <- ifelse(traitvar$min.I < 0, traitvar$mean.I/2, traitvar$min.I)

# combine data
modeled_data <- merge(H[,c("Site", "Date", "Mtot_H", "I_H")], R[,c("Site", "Date", "Mtot_R", "I_R")], by=c("Site", "Date"))
modeled_data <- merge(modeled_data, T[,c("Site", "Date", "Mtot_T", "I_T")], by=c("Site", "Date"))
modeled_data <- merge(modeled_data, THR[,c("Site", "Date", "Mtot_THR", "I_THR")], by=c("Site", "Date"))
modeled_data <- merge(modeled_data, traitvar, by=c("Site", "Date"))

# save data
write.csv(modeled_data, "Concatenated_Data/merged_model_data.csv", row.names = F)

