
# load libraries
library(rgdal)
library(raster)

# load data
countyShp <- readOGR("Kenya_historical/Kenya_counties/County.shp")
countyNames <- as.character(countyShp$COUNTY)

# aggregate rainfall data ---------------------------------
spatialRain <- data.frame(matrix(nrow=0,ncol=49))
colnames(spatialRain) <- c("Date", "Category", countyNames)
# write.csv(spatialRain, "Kenya_historical/spatialRain.csv", row.names=F)

rainfiles <- list.file("Kenya_historical/Kenya_ARC2_zips/")

rain_normal_start <- which(links == "africa_arc.19920901.tif.zip")
rain_normal_end <- which(links == "africa_arc.19931231.tif.zip")
rain_normal <- c(rain_normal_start:rain_normal_end)

rain_elnino_start <- which(links == "africa_arc.20140901.tif.zip")
rain_elnino_end <- which(links == "africa_arc.20151231.tif.zip")
rain_elnino <- c(rain_elnino_start:rain_elnino_end)

rain_lanina_start <- which(links == "africa_arc.19980901.tif.zip")
rain_lanina_end <- which(links == "africa_arc.19991231.tif.zip")
rain_lanina <- c(rain_lanina_start:rain_lanina_end)

# links <- c(rain_normal, rain_elnino, rain_lanina)
links <- c(rain_lanina)

for (i in 1:length(links)){
  filename <- paste0("Kenya_historical/Kenya_ARC2_zips/", links[i])
  raindat <- raster(filename)
  polymeans <- extract(raindat, countyShp, fun=mean)
  dateName <- paste(substr(links[i], 12,15), substr(links[i], 16,17), substr(links[i], 18,19), sep='-')
  if (links[i] %in% rain_normal){
    categ <- "Normal"
  } else if (links[i] %in% rain_elnino){
    categ <- "El_nino"
  } else if (links[i] %in% rain_lanina){
    categ <- "La_nina"
  }
  x <- c(dateName, categ, polymeans)
  write.csv(x, "Kenya_historical/spatialRain.csv", row.names=F, append=T)
}

# calculate cumulative rainfall
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
cumulativeRain <- read.csv("Kenya_historical/spatialTemp.csv", head=T, stringsAsFactors = F)
cumulativeRain <- cumulativeRain[grep("Date|Category|_cum$") names(cumulativeRain),]