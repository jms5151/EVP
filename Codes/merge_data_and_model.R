# merge observed data and modeled data ---------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(plyr)

# load case data
cases <- read.csv("Concatenated_Data/case_data/merged_case_data.csv", head=T, stringsAsFactors = F)

# load vector data
vectors <- read.csv("Concatenated_Data/vector_data/merged_vector_data.csv", head=T, stringsAsFactors = F)

# load modeled data
models <- read.csv("Concatenated_Data/merged_model_data.csv", head=T, stringsAsFactors = F)

# combine case data, vector data, and modeled data
data <- merge(cases, vectors, by=c("Site", "Date"), all=T)
data <- merge(data, models, by=c("Site", "Date"), all=T)

# calculate 7 and 30 day modeled means
data2 <- as.data.frame(matrix(ncol=16, nrow=nrow(data)))
colnames(data2) <- c("Mosq_7d_mean_T", "Cases_7d_mean_T", "Mosq_7d_mean_H", "Cases_7d_mean_H"
                     , "Mosq_7d_mean_R", "Cases_7d_mean_R", "Mosq_7d_mean_THR", "Cases_7d_mean_THR"
                     , "Mosq_30d_mean_T", "Cases_30d_mean_T", "Mosq_30d_mean_H", "Cases_30d_mean_H"
                     , "Mosq_30d_mean_R", "Cases_30d_mean_R", "Mosq_30d_mean_THR", "Cases_30d_mean_THR")
data <- cbind(data, data2)
data2 <- data[0,colnames(data)]

for (j in unique(data$Site)){
  temp.df <- subset(data, Site == j)
  for (k in 1:nrow(temp.df)){
    temp.df2 <- temp.df[k:(k+7),]
    temp.df$Cases_7d_mean_T[k] <- mean(temp.df2$I_T)
    temp.df$Cases_7d_mean_H[k] <- mean(temp.df2$I_H)
    temp.df$Cases_7d_mean_R[k] <- mean(temp.df2$I_R)
    temp.df$Cases_7d_mean_THR[k] <- mean(temp.df2$I_THR)
    temp.df2 <- temp.df[k:(k+29),]
    temp.df$Cases_30d_mean_T[k] <- mean(temp.df2$I_T)
    temp.df$Cases_30d_mean_H[k] <- mean(temp.df2$I_H)
    temp.df$Cases_30d_mean_R[k] <- mean(temp.df2$I_R)
    temp.df$Cases_30d_mean_THR[k] <- mean(temp.df2$I_THR)
    if (k >= 8){
      temp.df2 <- temp.df[(k-7):k,]
      temp.df$Mosq_7d_mean_T[k] <- mean(temp.df2$Mtot_T)
      temp.df$Mosq_7d_mean_H[k] <- mean(temp.df2$Mtot_H)
      temp.df$Mosq_7d_mean_R[k] <- mean(temp.df2$Mtot_R)
      temp.df$Mosq_7d_mean_THR[k] <- mean(temp.df2$Mtot_THR)
    }
    if (k >= 30){
      temp.df2 <- temp.df[(k-29):k,]
      temp.df$Mosq_30d_mean_T[k] <- mean(temp.df2$Mtot_T)
      temp.df$Mosq_30d_mean_H[k] <- mean(temp.df2$Mtot_H)
      temp.df$Mosq_30d_mean_R[k] <- mean(temp.df2$Mtot_R)
      temp.df$Mosq_30d_mean_THR[k] <- mean(temp.df2$Mtot_THR)
    }
  }
  data2 <- rbind(data2, temp.df)
}


# combine data
simobs <- merge(data[,1:34], data2[,c(1,2,35:50)], by=c("Site", "Date"), all.x=T)

# save data
write.csv(simobs, "Concatenated_Data/model_v_data.csv", row.names = F)