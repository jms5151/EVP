# land use index from "High-resolution multi-temporal mapping of global urban land using Landsat images based on the Google Earth Engine Platform"
# download: https://drive.google.com/drive/u/0/folders/0Bx0QqEEEYKQ3bS1BbjV6UVBWWms
rm(list=ls()) #remove previous variable assignments

# determine tiles in high resolution grid ---------------------
# create data frame of site locations
study_sites <- data.frame(Longitude = c(-80.2225, -79.9554, -79.6109, -79.6109, 34.636908, 34.767957, 39.668207, 39.566111)
                          , Latitude = c(-3.4764, -3.2581, -3.7173, -3.6573, -0.035266, -0.091702, -4.043477, -4.287500)
                          , Site = c("Huaquillas", "Machala", "Zaruma", "Portovelo", "Chulaimbo", "Kisumu", "Msambweni", "Ukunda"))

library(sf)
urban_shp <- st_read("Concatenated_data/urban_shapefiles/Data_grid.shp")
s.sp <- as(urban_shp, "Spatial")

library(rgdal)
pointtoplot <- study_sites
coordinates(pointtoplot) <- ~ Longitude + Latitude 
proj4string(pointtoplot) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

library(sp)
x <- pointtoplot %over% s.sp
study_sites$shp_ID <- x

# extract data from tiles in high resolution grid ------------
ids <- unique(study_sites$shp_ID[,])

library(raster)
study_sites$urban_index <- NA

for (i in ids){
  rowids <- which(study_sites$shp_ID == i)
  tmpdf <- study_sites[rowids,]
  urb_tile <- stack(paste0("Concatenated_data/urban_tiles/URBAN_2015_", i, ".tif"))
  x <- extract(urb_tile, cbind(tmpdf$Longitude, tmpdf$Latitude))
  study_sites$urban_index[rowids,] <- x
}
