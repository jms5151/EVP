################## travelAnalysis #####################
#################### 4/25/17 ##########################

## this code uses concatenated datasets created by Amy K. prior to use of redcap
## the first section finds coordinates for all travel routes for each child in study sites to make maps of travel density
## the second section finds unique coordinates for all cities that any child has travelled to

library(ggmap)
library(leaflet)
library(mapview)
library(raster)

##### SECTION 1: TRAVEL ROUTES
prevDenv <- read.csv("prev_denv_w_PCR_18_Apr_2017.csv", head=T)
inc <- read.csv("inc_denv_w_PCR_ 7_Apr_2017.csv", head=T)

prev <- prevDenv[,c("city", "wheretravel")]
incDenv <- inc[,c("city", "wheretravel")]

travel <- rbind(prev, incDenv)
travel2 <- travel[ !grepl("n /a", travel$wheretravel) , ]
travel2 <- travel2[ !grepl("n/a", travel2$wheretravel) , ]
travel2 <- travel2[ !grepl("n/ ", travel2$wheretravel) , ]  
travel2 <- travel2[ !grepl("n/a ", travel2$wheretravel) , ]
travel2 <- travel2[ !grepl("na", travel2$wheretravel) , ]
travel3 <- subset(travel2, wheretravel != 0)
travel3 <- subset(travel3, wheretravel != 2)
travel3 <- subset(travel3, wheretravel  != "")

library(splitstackshape)
travel5 <- cSplit(travel3, "wheretravel", " and ", "long")
travel5$wheretravel <- paste0(travel5$wheretravel, ", kenya")
travel5$wheretravel <- gsub("tanzania, kenya", "tanzania", travel5$wheretravel)
travel5$wheretravel <- gsub("uganda, kenya", "uganda", travel5$wheretravel)
travel5$wheretravel <- gsub("zanzibar, kenya", "zanzibar", travel5$wheretravel)
travel5$wheretravel <- gsub(", kenya, kenya", ", kenya", travel5$wheretravel)

travel5$longitude <- as.numeric(NA)
travel5$latitude <- as.numeric(NA)

## api only allows 2500 geocode queries per 24 hours
for (i in 1:nrow(travel5)){
  city <- as.character(travel5$wheretravel[i])
  coords <- geocode(city)
  travel5$longitude[i] <- coords[1]
  travel5$latitude[i] <- coords[2]
}

travel5$longitude <- unlist(travel5$longitude)
travel5$latitude <- unlist(travel5$latitude)
travel6 <- subset(travel5, longitude > -110) # an, kenya has wrong coordinates with longitude = -118.54635
write.csv(travel6, "Concatenated_Data/TravelRouteCoordinates_2017_05_01.csv", row.names=F)

##################### Make maps of travel routes ##########################
#chulaimbo
# longitude: 34.633333
# latitude: -.033333
chulaimbo <- subset(travel5, city=="chulaimbo")
chulaimbo2 <- chulaimbo[!is.na(chulaimbo$latitude),]
root <- matrix(c(34.633333, -.033333), ncol = 2)
colnames(root) <- c("longitude", "latitude")

## end points
locations <- chulaimbo2[,c("longitude", "latitude")]
locations$longitude <-  unlist(locations$longitude)
locations$latitude <- unlist(locations$latitude)
# locations2 <- subset(locations, longitude > 30 & longitude < 39)
# locations2 <- subset(locations2, latitude > -5 & latitude < 7)
  
## create and append spatial lines
lst <- lapply(1:nrow(locations), function(i) {
  SpatialLines(list(Lines(list(Line(rbind(root, locations[i, ]))), ID = i)), 
               proj4string = CRS("+init=epsg:4326"))
})

sln <- do.call("bind", lst)

## display data
mapview(sln)
#####################
#msambweni
# Latitude : -4.467
# Longitude : 39.483
msambweni <- subset(travel5, city=="msambweni")
msambweni2 <- msambweni[!is.na(msambweni$latitude),]
root <- matrix(c(39.483, -4.467), ncol = 2)
colnames(root) <- c("longitude", "latitude")

## end points
locations <- msambweni2[,c("longitude", "latitude")]
locations$longitude <-  unlist(locations$longitude)
locations$latitude <- unlist(locations$latitude)
# locations2 <- subset(locations, longitude > 6)
# locations2 <- subset(locations2, latitude > 0)

## create and append spatial lines
lst <- lapply(1:nrow(locations), function(i) {
  SpatialLines(list(Lines(list(Line(rbind(root, locations[i, ]))), ID = i)), 
               proj4string = CRS("+init=epsg:4326"))
})

sln <- do.call("bind", lst)

## display data
mapview(sln)

#####################
#kisumu
# Latitude	-0.091702
# Longitude	34.767956
kisumu <- subset(travel6, city=="kisumu")
kisumu2 <- kisumu[!is.na(kisumu$latitude),]
root <- matrix(c(34.767956, -0.091702), ncol = 2)
colnames(root) <- c("longitude", "latitude")

## end points
locations <- kisumu2[,c("longitude", "latitude")]
locations$longitude <-  unlist(locations$longitude)
locations$latitude <- unlist(locations$latitude)

## create and append spatial lines
lst <- lapply(1:nrow(locations), function(i) {
  SpatialLines(list(Lines(list(Line(rbind(root, locations[i, ]))), ID = i)), 
               proj4string = CRS("+init=epsg:4326"))
})

sln <- do.call("bind", lst)

## display data
mapview(sln)

#####################
#ukunda
# Latitude	-4.2879
# Longitude	39.5653
ukunda <- subset(travel5, city=="ukunda")
ukunda2 <- ukunda[!is.na(ukunda$latitude),]
root <- matrix(c(39.5653, -4.2879), ncol = 2)
colnames(root) <- c("longitude", "latitude")

## end points
locations <- ukunda2[,c("longitude", "latitude")]
locations$longitude <-  unlist(locations$longitude)
locations$latitude <- unlist(locations$latitude)

## create and append spatial lines
lst <- lapply(1:nrow(locations), function(i) {
  SpatialLines(list(Lines(list(Line(rbind(root, locations[i, ]))), ID = i)), 
               proj4string = CRS("+init=epsg:4326"))
})

sln <- do.call("bind", lst)

## display data
mapview(sln)

##### SECTION 2: UNIQUE CITY COORDINATES
kenyaCoordinates <- travel5[,c("wheretravel", "longitude", "latitude")] 
kenyacoordinates2 <- unique(kenyaCoordinates)
kenyacoordinates2$wheretravel <- gsub("alego siaya, kenya", "alego, siaya, kenya", kenyacoordinates2$wheretravel)
kenyacoordinates2$wheretravel <- gsub("alendo-kanuu, kenya|alendu-kanuu, kenya|alundu-kannu, kenya", "alendu-kanuu, kenya", kenyacoordinates2$wheretravel)
kenyacoordinates2$wheretravel <- gsub("asembo oyude, kenya", "asembo, kenya", kenyacoordinates2$wheretravel)
kenyacoordinates2$wheretravel <- gsub("gem gombe, kenya", "gombe, kenya", kenyacoordinates2$wheretravel)
kenyacoordinates2$wheretravel <- gsub("gem yala, kenya|gem-yala, kenya", "yala, kenya", kenyacoordinates2$wheretravel)
kenyacoordinates2$wheretravel <- gsub("imbo-bondo, kenya", "bondo, kenya", kenyacoordinates2$wheretravel) # this could be imbo or bondo but both already exist so this entry would be a duplicate regardless
kenyacoordinates2$wheretravel <- gsub("kabwoch, kenya", "kabwoch, uganda", kenyacoordinates2$wheretravel)
kenyacoordinates2$wheretravel <- gsub("kigombero, kenya", "kigombero, tanzania", kenyacoordinates2$wheretravel)
kenyacoordinates2$wheretravel <- gsub("koru-mworoni, kenya", "koru, kenya", kenyacoordinates2$wheretravel) # this could also be mworoni, they are very close together
kenyacoordinates2$wheretravel <- gsub("likoni,duga, kenya", "likoni, kenya", kenyacoordinates2$wheretravel)
kenyacoordinates2$wheretravel <- gsub("mawego karachuonyo, kenya", "mawego, kenya", kenyacoordinates2$wheretravel)
kenyacoordinates2$wheretravel <- gsub("mikindani,boi,magongo,taita, kenya", "mombasa, kenya", kenyacoordinates2$wheretravel) # all refer to different parts of mombasa
kenyacoordinates2$wheretravel <- gsub("n, kenya|n/, kenya", NA, kenyacoordinates2$wheretravel)
kenyacoordinates2$wheretravel <- gsub("tanzania, mombasa, kenya", "tanzania", kenyacoordinates2$wheretravel) # both tanzania and mombasa are duplicates
kenyacoordinates2$wheretravel <- gsub("n, kenya|n/, kenya", NA, kenyacoordinates2$wheretravel)
kenyacoordinates2$wheretravel <- gsub("n, kenya|n/, kenya", NA, kenyacoordinates2$wheretravel)
kenyacoordinates2$wheretravel <- gsub("n, kenya|n/, kenya", NA, kenyacoordinates2$wheretravel)
kenyacoordinates3 <- kenyacoordinates2[!duplicated(kenyacoordinates2$wheretravel),]
kenyaCoordinatesExist <- kenyacoordinates3[!is.na(kenyacoordinates3$longitude),]
kenyaCoordinatesNA<- subset(kenyacoordinates3, is.na(kenyacoordinates3$longitude))

for (i in 1:nrow(kenyaCoordinatesNA)){
  city <- as.character(kenyaCoordinatesNA$wheretravel[i])
  coords <- geocode(city)
  kenyaCoordinatesNA$longitude[i] <- coords[1]
  kenyaCoordinatesNA$latitude[i] <- coords[2]
}

uniqueTravelCoordinates <- rbind(kenyaCoordinatesExist, kenyaCoordinatesNA)
bridgeTravelNames <- unique(travel$wheretravel)
loc <- read.csv("uniqueTravel.csv", head=T)
colnames(uniqueTravelCoordinates)[1] <- "travel_location"
bridgeTravel <- merge(loc, uniqueTravelCoordinates, by="travel_location", all.x=T)
trav <- bridgeTravel[c(1:342),]
trav$longitude <- unlist(trav$longitude)
trav$latitude <- unlist(trav$latitude)
write.csv(trav, "Concatenated_Data/TravelCoordinates_2017_05_01.csv", row.names=F)
