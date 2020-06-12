# measure urbanization
rm(list=ls()) #remove previous variable assignments

# load libraries
library(raster)

# load data
nightLights <- brick("C:/Users/Jamie/Box/Projects/2020_Caldwell_etal_Nature_Communications/Concatenated_Data/BlackMarble_2016_3km_geo.tif")

# create data frame of site locations
study_sites <- data.frame(Longitude = c(-80.2225, -79.9554, -79.6109, -79.6109, 34.636908, 34.767957, 39.668207, 39.566111)
                          , Latitude = c(-3.4764, -3.2581, -3.7173, -3.6573, -0.035266, -0.091702, -4.043477, -4.287500)
                          , Site = c("Huaquillas", "Machala", "Zaruma", "Portovelo", "Chulaimbo", "Kisumu", "Msambweni", "Ukunda"))

# extract nighttime lights data
urban_index <- extract(nightLights, cbind(study_sites$Longitude, study_sites$Latitude))

# merge data
urbanization <- cbind(study_sites, "urban_index" = urban_index[,1])

# save data
save(urbanization, file="data/urban_index.RData")