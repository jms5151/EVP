########## amy's prevalence data 4/21/17 ############

prevDenv <- read.csv("prev_denv_w_PCR_18_Apr_2017.csv", head=T)

denV <- prevDenv[,c("date", "house_latitude", "latitude", "compound_latitude", "house_longitude", "longitude", "compound_longitude",
                    "studyid", "stanfordchikvigg_", "stanforddenvigg_",
                    "city", "houseid", "age", "gender", "childoccupation", "numsiblings",
                    "childtravel", "wheretravel", "nightaway", "outdooractivity", "hrsoutdoors",
                    "watercollectitems", "flooring", "livestock_location", "latrine_distance",
                    "water_containers", "school")]

denV$Date <- as.Date(denV$date, "%d-%b-%y")
denV$scaledAge <- scale(denV$age) # mean age = 6.4yo
denV$childoccupation[is.na(denV$childoccupation)] <- "Other" # make NA/99s 6 so not so far out of range
denV$childoccupation[which(denV$childoccupation == 1)] <- "No_school"
denV$childoccupation[which(denV$childoccupation == 2)] <- "Madrassa"
denV$childoccupation[which(denV$childoccupation == 3)] <- "Nursery_school"
denV$childoccupation[which(denV$childoccupation == 4)] <- "Primary_school"
denV$childoccupation[which(denV$childoccupation == 5)] <- "Secondary_school"
denV$childoccupation[which(denV$childoccupation == 6)] <- "Other_student"
denV$childoccupation[which(denV$childoccupation == 99)] <- "Other2"
denV$childoccupation[which(denV$childoccupation == 8)] <- "Herder"
denV$childoccupation[which(denV$childoccupation == 10)] <- "Other"

#denV$scaledOccupation <- scale(denV$childoccupation) # effect size doesn't change if scaled but intercept does
fit.1 <- glm(denV$stanforddenvigg_ ~ denV$scaledAge + denV$city + denV$childoccupation, family=binomial(link="logit"))
summary(fit.1)


denV2 <- subset(denV, (!is.na(denV[,2])) & (!is.na(denV[,3]))) 
denV$lat <- apply(denV[,2:3], 1, function(i) ifelse(all(is.na(i)), NA, i[!is.na(i)]))
test <- denV[,c("latitude", "house_latitude", "lat")]

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
denV$house <- paste0("Age: ", denV$age,
                       "<br> Occupation: ", denV$childoccupation)

pal = colorNumeric(
  palette = "Greens",
  domain = denV$stanforddenvigg_
)
factpal <- colorFactor(topo.colors(2), denV$stanforddenvigg_)
# plot Ukunda as satellite data
denvMap <- leaflet() %>% 
  addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>%
  addPolygons(data=denV, lat = ~house_latitude, lng = ~house_longitude, popup = ~house))
addScaleBar(denvMap)

