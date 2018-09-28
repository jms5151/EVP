rm(list = ls()) # remove everything stored in environment

#-- load libraries
library(sp)
library(rgdal)
library(ggmap)
library(GISTools)

df <- data.frame("Longitude"=as.numeric(NA), "Latitude"=as.numeric(NA))
df[1,] <- c(34.636908, -0.035266) #chulaimbo
df[2,] <- c(34.767956, -0.091702) #kisumu
df[3,] <- c(39.4813, -4.4653) #msambweni
df[4,] <- c(39.5653, -4.2879) #ukunda

location=c(37,0) # set the center of the map
map1 <- get_map(location=location,crop = F,
                color="bw",
                maptype="terrain",
                source="google",
                zoom=6)
sitemap <- ggmap(map1, extent = 'panel') # use the BW terrain option

nicemap<- 
  sitemap + 
  labs(x="Longitude", y="Latitude") + 
  geom_point(data=df, aes(x=Longitude, y=Latitude, 
                               col="black", fill="yellow"), 
             pch=21, size=4)+
  scale_fill_manual(values=c("lightblue", "darkblue")) +
  scale_colour_manual(values=c("black", "black"))+
  theme_bw()+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

nicemap
