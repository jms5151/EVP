rm(list = ls()) # remove everything stored in environment

prev_chick <- read.csv("prev_chikv_w_PCR_24_Mar_2017.csv", head=T, stringsAsFactors = F)

ovi <- read.csv("VectorData/coastVector/Ovitrapdata.csv", head=T, stringsAsFactors = F)
ovi$Egg.count <- as.numeric(ovi$Egg.count)
months <- read.csv("ovi_months.csv", head=T)
ovi <- merge(ovi, months, by="Month")
ovi$stEggs <- ovi$Egg.count/length(ovi$House.ID)

# x <- unique(ovi$Month)
# write.csv(x, "ovi_months.csv", row.names = F)
# sum(na.omit(prev_chick$stanforddenvigg_))
plot(prev_chick$ai, prev_chick$stanforddenvigg_)
library(ggplot2)
ggplot(prev_chick, aes(x = childoccupation, y = stanforddenvigg_)) + geom_bar(stat = 'identity')
unique(prev_chick$)
hcc <- subset(prev_chick, hccparticipant == 1)
class(prev_chick$hccparticipant)
length(unique(prev_chick$studyid))
sum(na.omit(prev_chick$hccparticipant == NA))
plot(prev_chick$childoccupation, prev_chick)
sum(na.omit(prev_chick$childtravel))
unique(prev_chick$wheretravel)
boxplot(Egg.count~month, data=ovi)
hist(ovi$Egg.count, breaks=20)

bg <- read.csv("VectorData/coastVector/BG_Sentinel_data.csv", head=T, stringsAsFactors = F)
hlc <- read.csv("VectorData/coastVector/HLC.csv", head=T, stringsAsFactors = F)

