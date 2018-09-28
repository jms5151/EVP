rm(list = ls()) # remove everything stored in environment

ukunda <- read.csv("CoastCleaned/coast_demography/Ukunda_HCC_Houses_coordinates_Mar17.csv", head=T, stringsAsFactors = F)
ukunda[,c("Latitude", "Longitude")] = apply(ukunda[,c("Latitude", "Longitude")], 2, function(x) as.numeric(x)) # make lat/lon coordinate numeric

# ukundaChild <- read.csv("CoastCleaned/coast_demography/Ukunda_HCC_children_demography_Mar17.csv", head=T, stringsAsFactors = F)
# ukundaChild[,c("Latitude", "Longitude")] = apply(ukundaChild[,c("Latitude", "Longitude")], 2, function(x) as.numeric(x)) # make lat/lon coordinate numeric

msambweni <- read.csv("CoastCleaned/coast_demography/msambweni_hh_tomap.csv", head=T, stringsAsFactors = F)
msambweni <- msambweni[complete.cases(msambweni),]
#msambweni <- read.csv("CoastCleaned/coast_demography/Msambweni_coordinates_Nov21_2016.csv", head=T, stringsAsFactors = F)
chulaimbo <- read.csv("WestCleaned/west_demography/Chulaimbo_Demography_Data.csv", head=T, stringsAsFactors = F)
kisumu <- read.csv("WestCleaned/west_demography/Kisumu_Demography_Data.csv", head=T, stringsAsFactors = F)

library(leaflet) # load library

## Add scale bar that adjusts with zoom function
addScaleBar = function(map,
                       position = c('topright', 'bottomright', 'bottomleft', 'topleft'),
                       options = scaleBarOptions()) {
  
  options = c(options, list(position = match.arg(position)))
  invokeMethod(map, getMapData(map), 'addScaleBar', options)
}

scaleBarOptions = function(maxWidth = 100, metric = TRUE, imperial = TRUE,
                           updateWhenIdle = TRUE) {
  list(maxWidth=maxWidth, metric=metric, imperial=imperial,
       updateWhenIdle=updateWhenIdle)
}

# add pop up data
ukunda$house <- paste0("Age: ", ukunda$FamilyName,
                       "<br> Age: ", ukunda$Age, " years",
                       "<br> Gender: ", ukunda$HH_gender,
                       "<br> Roof type: ", ukunda$Roof)

# plot Ukunda as satellite data
ukundaMap <- leaflet() %>% # %>% addTiles() %>% code for open maps rather than satellite, comment line below for open maps
  addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>%
  addCircles(data=ukunda, lat = ~Latitude, lng = ~Longitude, popup = ~house)
addScaleBar(ukundaMap)

#plot UkundaChild data
# ukundaChildMap <- leaflet() %>% 
#   addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>%
#   addCircles(data=ukundaChild, lat = ~Latitude, lng = ~Longitude)
# addScaleBar(ukundaChildMap)

#plot Chulaibo data
chulaimbo$house <- paste0("Unique key: ", chulaimbo$unique_key,
                       "<br> Head of household surname: ", chulaimbo$head_of_householdhoh_namehoh_sur,
                       "<br> HOH gender: ", chulaimbo$head_of_householdgender,
                       "<br> Latitude: ", chulaimbo$gps_houselatitude,
                       "<br> Longitude: ", chulaimbo$gps_houselongitude)

chulaimboMap <- leaflet() %>% 
  addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>%
  addCircles(data=chulaimbo, lat = ~gps_houselatitude, lng = ~gps_houselongitude, popup = ~house)
addScaleBar(chulaimboMap)

#plot Kisumu data
kisumu$house <- paste0("Unique key: ", kisumu$unique_key,
                       "<br> Village: ", kisumu$village,
                       "<br> Latitude: ", kisumu$gps_houselatitude,
                       "<br> Longitude: ", kisumu$gps_houselongitude)

kisumuMap <- leaflet() %>% 
  addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>%
  addCircles(data=kisumu, lat = ~gps_houselatitude, lng = ~gps_houselongitude, popup = ~house)
addScaleBar(kisumuMap)

## convert Msambweni easting/northing coordinates to lat/lon
# library(proj4)
library(rgdal) #load package libraries

# sputm <- SpatialPoints(coords, proj4string=CRS("+proj=utm +zone=36S +datum=WGS84"))
# spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))

latlong = "+init=epsg:4326"
msambweni$newID <- 1:nrow(msambweni)
msambweni <- subset(msambweni, X != "" | Y != "") # Remove missing Eastings or Northings

# Create coordinates variable
coords <- cbind(Easting = msambweni$X, Northing = msambweni$Y)

# Create the SpatialPointsExpandedSurveysFrame
gpsMSAM <- SpatialPointsDataFrame(coords, data = data.frame(msambweni$newID), 
                                  proj4string = CRS("+init=epsg:32737"))

# Find epsg code: http://spatialreference.org/ref/epsg/?search=NAD83+4N&srtext=Search
# ESPG for NAD83(HARN) / UTM zone 37S = 32737 (or 36S = 32736)

# Convert from Eastings and Northings to Latitude and Longitude
gpsMSAM_LL <- spTransform(gpsMSAM, CRS(latlong))

# we also need to rename the columns
colnames(GP_SP_LL@coords)[colnames(GP_SP_LL@coords) == "Easting"] <- "Longitude"
colnames(GP_SP_LL@coords)[colnames(GP_SP_LL@coords) == "Northing"] <- "Latitude"

GPS$survey <- paste(GPS$Site, GPS$In.Out, GPS$Transect, GPS$Date, sep='-') #concatenate identifiers of survey 

#plot Msambweni data
msambweniMap <- leaflet() 
  # addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>%
  addCircles(data=msambweni, lat = ~POINT_Y, lng = ~POINT_X)
addScaleBar(msambweniMap)

### find houses without lat/lon coordinates
u <- subset(ukunda, Latitude == ".")
uk <- u[,c("village", "Villhouse")]
colnames(uk)[2] <- "unique_key"
c <- chulaimbo[is.na(chulaimbo$gps_houselatitude),]
ch <- c[,c("village", "unique_key")]
missingData <- rbind(uk, ch)
k <- kisumu[is.na(kisumu$gps_houselatitude),]
ki <- k[,c("village", "unique_key")]
missingData <- rbind(missingData, ki)
write.csv(missingData, "missingCoordinates.csv", row.names=F)
#m <- msambweni[is.na(msambweni$POINT_X),]
#ms <- m[,c("")]



### prevalence mapped
prevDENV <- read.csv("prev_denv_w_PCR_18_Apr_2017.csv", head=T)

denV <- prevDENV[,c("stanforddenvigg_", "site", "latitude", "house_latitude", "longitude", "house_longitude")]

denPos <- subset(denV, stanforddenvigg_ == 1)
# denPos$Latitude <- do.call(pmax, c(denPos[,c("latitude", "house_latitude")], list(na.rm=TRUE)))
# denPos$Longitude <- do.call(pmax, c(denPos[,c("longitude", "house_longitude")], list(na.rm=TRUE)))
denPos$Latitude
denPos$Latitude <- gsub(-.033333, "Chulaimbo", denPos$site)
denPos$Longitude <- gsub(34.633333, "Chulaimbo", denPos$site)


prevMap <- leaflet() %>% 
  addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>%
  addCircles(data=denPos, lat = ~Latitude, lng = ~Longitude, color="yellow")
addScaleBar(prevMap)