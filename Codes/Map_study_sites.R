# https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

source("C:/Users/Jeremy/Box Sync/R_functions/Google_maps_API_token.R")

# set sites
ecuador.sites <-data.frame(longitude = c(-80.2225, -79.9554, -79.6109, -79.6109)
                           , latitude = c(-3.4764, -3.2581, -3.7173, -3.6573)
                           , Site = c("Huaquillas", "Machala", "Zaruma", "Portovelo"))

kenya.sites <- data.frame(longitude = c(34.636908, 34.767957, 39.668207, 39.566111)
                          , latitude = c(-0.035266, -0.091702, -4.043477, -4.287500)
                          , Site = c("Chulaimbo", "Kisumu", "Msambweni", "Ukunda"))

# get basemaps
ecuador <- get_googlemap(center = c(-79.6109, -2.7173), maptype = "terrain", source = "google", zoom = 8, style='feature:all|element:labels|visibility:off')
kenya <- get_googlemap(center = c(37.9083, 0.1769), maptype = "terrain", source = "google", zoom = 6, style='feature:all|element:labels|visibility:off')

# make maps
ggmap(ecuador) +
  geom_point(data = ecuador.sites, mapping = aes(x = longitude, y = latitude)) +
  geom_label_repel(aes(x = longitude, y = latitude, label = Site)
                   , xlim = c(ecuador.sites$longitude,0.05), ylim = c(ecuador.sites$latitude,0.05)
                   , min.segment.length = 0.5, direction='both'
                   , data = ecuador.sites) + 
  ggtitle("A. Ecuador") +
  ylab("Latitude") +
  xlab("Longitude")

ggmap(kenya) +
  geom_point(data = kenya.sites, mapping = aes(x = longitude, y = latitude)) +
  geom_label_repel(aes(x = longitude, y = latitude, label = Site)
                   , xlim = c(kenya.sites$longitude,0.05), ylim = c(kenya.sites$latitude,0.05)
                   , min.segment.length = 0.5, direction='both'
                   , data = kenya.sites) + 
  ggtitle("B. Kenya") +
  ylab("Latitude") +
  xlab("Longitude")










# -------------------------------------------------------------------------------------------
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-3.html ----------------------------------

# load libraries
library("cowplot")
library("googleway")
library("ggplot2")
library("ggrepel")
library("ggspatial")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("rgeos")

# set theme and get world map
theme_set(theme_bw())
world <- ne_countries(scale='medium', returnclass = 'sf')

# set study site coordinates
kenya.sites <- st_as_sf(data.frame(longitude = c(34.636908, 34.767957, 39.668207, 39.566111)
                        , latitude = c(-0.035266, -0.091702, -4.043477, -4.287500))
                        , coords = c("longitude", "latitude"), crs = 4210, agr = "constant")

ecuador.sites <- st_as_sf(data.frame(longitude = c(-80.2225, -79.9554, -79.6109, -79.6109)
                          , latitude = c(-3.4764, -3.2581, -3.7173, -3.6573)) # last latitude is actually -3.6873; I shifted it for visualization 
                          , coords = c("longitude", "latitude"), crs = 4248, agr = "constant")

# Map of world with insets for Ecuador and Kenya -----------------------------------------
gworld <- ggplot(data = world) +
  geom_sf(aes(fill = region_wb)) + #, show.legend = FALSE
  geom_rect(xmin = -83, xmax = -73, ymin = -7, ymax = 4,
            fill = NA, colour = "black", size = 1.5) +
  geom_rect(xmin = 31, xmax = 45, ymin = -6.5, ymax = 6.5,
              fill = NA, colour = "black", size = 1.5) +
  scale_fill_viridis_d(option = "plasma", name="") +
  theme(panel.background = element_rect(fill = "azure"),
        panel.border = element_rect(fill = NA))

kenyamap <- ggplot(data = world) +
  geom_sf(aes(fill = region_wb)) +
  annotate(geom = "text", x = 37.7, y = 2, label = "Kenya", fontface = "bold", color = "grey22", size = 4) +
  geom_sf(data = kenya.sites, size = 2, shape = 21, fill = "black", color='white') +
  coord_sf(xlim = c(33, 43), ylim = c(-5, 5.5), expand = FALSE) +
  scale_fill_viridis_d(option = "plasma") +
  theme(legend.position = "none", axis.title.x = element_blank(), 
        axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks = element_blank(), panel.background = element_rect(fill = "azure"), 
        panel.border = element_rect(fill = NA), plot.background = element_blank())

ecuadormap <- ggplot(data = world) +
  geom_sf(aes(fill = region_wb)) +
  annotate(geom = "text", x = -78.1, y = -0.5, label = "Ecuador", fontface = "bold", color = "grey22", size = 4) +
  geom_sf(data = ecuador.sites, size = 2, shape = 21, fill = "black", color='white') +
  coord_sf(xlim = c(-81, -75), ylim = c(-5.1, 1.6), expand = FALSE) +
  scale_fill_viridis_d(option = "plasma") +
  theme(legend.position = "none", axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks = element_blank(), panel.background = element_rect(fill = "azure"),
        panel.border = element_rect(fill = NA), plot.background = element_blank())

gworld +
  annotation_custom(grob = ggplotGrob(ecuadormap),
    xmin = -210, xmax = -75,
    ymin = -90, ymax = 25) +
  annotation_custom(grob = ggplotGrob(kenyamap),
    xmin = 40, xmax = 175,
    ymin = -50, ymax = 65)

# Map of Ecuador and Kenya -------------------------------------------------------------
gworld <- ggplot(data = world) +
  geom_sf(fill = "transparent", show.legend = FALSE) +
  geom_rect(xmin = -83, xmax = -73, ymin = -7, ymax = 4,
            fill = NA, colour = "black", size = 1.5) +
  geom_rect(xmin = 31, xmax = 45, ymin = -6.5, ymax = 6.5,
            fill = NA, colour = "black", size = 1.5) +
  scale_fill_viridis_d(option = "plasma", name="") +
  theme(panel.background = element_rect(fill = NA), panel.border = element_rect(fill = NA))

kenyamap2 <- ggplot(data = world) +
  geom_sf(aes(fill = region_wb)) +
  annotate(geom = "text", x = 37.7, y = 2, label = "Kenya", fontface = "bold", color = "grey22", size = 6) +
  annotate(geom = "text", x = 35, y = 0.25, label = "Chulaimbo", fontface = "italic", color = "grey22", size = 4) +
  annotate(geom = "text", x = 35.1, y = -0.35, label = "Kisumu", fontface = "italic", color = "grey22", size = 4) +
  annotate(geom = "text", x = 38.6, y = -3.8, label = "Msambweni", fontface = "italic", color = "grey22", size = 4) +
  annotate(geom = "text", x = 38.7, y = -4.2, label = "Ukunda", fontface = "italic", color = "grey22", size = 4) +
  geom_sf(data = kenya.sites, size = 2, shape = 21, fill = "black", color='white') +
  coord_sf(xlim = c(33, 43), ylim = c(-5, 5.5), expand = FALSE) +
  scale_fill_viridis_d(option = "plasma") +
  theme(legend.position = "none", axis.title.x = element_blank(), 
        axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
        panel.border = element_rect(fill = NA))

ecuadormap2 <- ggplot(data = world) +
  geom_sf(aes(fill = region_wb)) +
  annotate(geom = "text", x = -78.1, y = -0.5, label = "Ecuador", fontface = "bold", color = "grey22", size = 6) +
  annotate(geom = "text", x = -79.3, y = -3.2, label = "Machala", fontface = "italic", color = "grey22", size = 4) +
  annotate(geom = "text", x = -79.5, y = -3.45, label = "Huaquillas", fontface = "italic", color = "grey22", size = 4) +
  annotate(geom = "text", x = -79.1, y = -3.65, label = "Zaruma", fontface = "italic", color = "grey22", size = 4) +
  annotate(geom = "text", x = -79, y = -3.85, label = "Portovelo", fontface = "italic", color = "grey22", size = 4) +
  geom_sf(data = ecuador.sites, size = 2, shape = 21, fill = "black", color='white') +
  coord_sf(xlim = c(-81, -75), ylim = c(-5.1, 1.6), expand = FALSE) +
  scale_fill_viridis_d(option = "plasma") +
  theme(legend.position = "none", axis.title.x = element_blank(),
        axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"),
        panel.border = element_rect(fill = NA))

ggplot() +
  coord_equal(xlim = c(0, 2), ylim = c(0, 1), expand = FALSE) +
  annotation_custom(ggplotGrob(ecuadormap2), xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
  annotation_custom(ggplotGrob(kenyamap2), xmin = 1, xmax = 2, ymin = 0, ymax = 1) +
  theme_void()

ggplot() +
  coord_equal(xlim = c(0, 2), ylim = c(0, 2), expand = FALSE) +
  annotation_custom(ggplotGrob(ecuadormap2), xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
  annotation_custom(ggplotGrob(kenyamap2), xmin = 1, xmax = 2, ymin = 0, ymax = 1) +
  annotation_custom(ggplotGrob(gworld), xmin = 0, xmax = 2, ymin = 1, ymax = 2) +
  theme_void()


