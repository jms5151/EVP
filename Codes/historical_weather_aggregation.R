
# load libraries
library(rgdal)
library(raster)
library(tidyr)
library(plyr)

# load data
countyShp <- readOGR("Kenya_historical/Kenya_counties/County.shp")
countyNames <- as.character(countyShp$COUNTY)

# aggregate rainfall data ---------------------------------
spatialRain <- data.frame(matrix(nrow=0,ncol=49))
colnames(spatialRain) <- c("Date", "Category", countyNames)
# write.csv(spatialRain, "Kenya_historical/spatialRain.csv", row.names=F)

rainfiles <- list.files("Kenya_historical/Kenya_ARC2_zips/")

rain_normal_start <- which(rainfiles == "africa_arc.19920901.tif")
rain_normal_end <- which(rainfiles == "africa_arc.19931231.tif")
rain_normal <- rainfiles[rain_normal_start:rain_normal_end]

rain_elnino_start <- which(rainfiles == "africa_arc.20140901.tif")
rain_elnino_end <- which(rainfiles == "africa_arc.20151231.tif")
rain_elnino <- rainfiles[rain_elnino_start:rain_elnino_end]

rain_lanina_start <- which(rainfiles == "africa_arc.19980901.tif")
rain_lanina_end <- which(rainfiles == "africa_arc.19991231.tif")
rain_lanina <- rainfiles[rain_lanina_start:rain_lanina_end]

# links <- c(rain_normal, rain_elnino, rain_lanina)
links <- c(rain_lanina)

for (i in 2:length(links)){
  filename <- paste0("Kenya_historical/Kenya_ARC2_zips/", links[i])
  raindat <- raster(filename)
  polymeans <- extract(raindat, countyShp, fun=mean)
  dateName <- paste(substr(links[i], 12,15), substr(links[i], 16,17), substr(links[i], 18,19), sep='-')
  # if (links[i] %in% rain_normal){
  #   categ <- "Normal"
  # } else if (links[i] %in% rain_elnino){
  #   categ <- "El_nino"
  # } else if (links[i] %in% rain_lanina){
  #   categ <- "La_nina"
  # }
  categ <- "La_nina"
  spatialRain[nrow(spatialRain)+1,] <- c(dateName, categ, polymeans)
  write.table(spatialRain[nrow(spatialRain),], "Kenya_historical/spatialRain.csv", row.names=F, append=T, quote= FALSE, sep=",", col.names=F)
  cat(i, "-", dateName, "\n")
}

# calculate cumulative rainfall
spatialRain <- read.csv("Kenya_historical/spatialRain.csv", head=T, stringsAsFactors = F)
spatialRain[,3:49] <- lapply(spatialRain[,3:49], as.numeric)
cumulativeRain <- spatialRain

for (j in 1:length(countyNames)){
  cumulativeRain[paste0(countyNames[j], "_cum")] <- NA
  for (k in 30:nrow(cumulativeRain)){
    cumulativeRain[k,paste0(countyNames[j], "_cum")] <- sum(cumulativeRain[(k-29):k, countyNames[j]])
  }  
}  

write.csv(cumulativeRain, "Kenya_historical/cumulativeRain.csv", row.names=F)

# combine temperature and rainfall data -------------------------------------
spatialTemp <- read.csv("Kenya_historical/spatialTemp.csv", head=T, stringsAsFactors = F)
cumulativeRain <- read.csv("Kenya_historical/cumulativeRain.csv", head=T, stringsAsFactors = F)
cumulativeRain <- cumulativeRain[,grep("Date|Category|_cum$", names(cumulativeRain))]
colnames(cumulativeRain) <- gsub("_cum", "", colnames(cumulativeRain))

# shape data from wide to long
tempDF <- spatialTemp %>% gather(key="County", value="Temperature", 3:49)
rainDF <- cumulativeRain %>% gather(key="County", value="Monthly_rainfall", 3:49)

# fill in missing rainfall values
meanRainVals <- ddply(rainDF, .(County, Category), summarize, rainmeanval = ifelse(all(Monthly_rainfall==0)==TRUE, NA, mean(Monthly_rainfall, na.rm=T)))  
countiesToRemove <- unique(meanRainVals$County[is.na(meanRainVals$rainmeanval)])

# merge data and remove counties where rainfall always equals zero
combinedTR <- merge(tempDF, rainDF, by=c("Date", "Category", "County"), all.x=T)
combinedTR <- merge(combinedTR, meanRainVals, by=c("County", "Category"))
combinedTR$Monthly_rainfall <- ifelse(is.na(combinedTR$Monthly_rainfall), combinedTR$rainmeanval, combinedTR$Monthly_rainfall)
combinedTR <- combinedTR[!(combinedTR$County %in% countiesToRemove),]
combinedTR <- combinedTR[order(combinedTR$County, combinedTR$Date), c("County", "Category", "Date", "Temperature", "Monthly_rainfall")]

# format date and save data
combinedTR$Date <- as.Date(combinedTR$Date, "%Y-%m-%d")
save(combinedTR, file="Kenya_historical/combinedTR.RData")
