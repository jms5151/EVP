# map dengue relationships from literature ---------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries -----------------------------------------------------------------------
library("spam")
library("rworldmap")
library("RColorBrewer")

# load data ----------------------------------------------------------------------------
dengueData <- read.csv("C:/Users/Jamie/Box Sync/Stanford/Msc_presentations/map_dengue_relationships/MDB_Climate_Review.csv", head=T)

# subset data --------------------------------------------------------------------------
dengueTemps <- subset(dengueData, Climate_variable == "Temperature" & !is.na(Relationship_to_vectors_in_best_fit_model))
dengue.countries <- dengueTemps[,c("Study_location", "Relationship_to_vectors_in_best_fit_model")]
dengue.countries <- dengue.countries[!duplicated(dengue.countries),]

# make map -----------------------------------------------------------------------------
d <- data.frame(country=c("China", "Ecuador", "Taiwan", "Nepal", "Pakistan", "Brazil", "Argentina", "USA", "New Caledonia"),
                value=c(1,2,2,2,3,3,3,3,4)) # 1=negative, 2=non-linear, 3=positive, 4=none

n <- joinCountryData2Map(d, joinCode="NAME", nameJoinColumn="country")

# colourPalette <- brewer.pal(4,"YlGnBu")
colourPalette <- c("olivedrab2", "deepskyblue1", "firebrick1", "darkblue")

mapCountryData(n, nameColumnToPlot="value", mapTitle="", colourPalette=colourPalette
               , addLegend=FALSE, border="black",catMethod='fixedWidth', numCats=4)

# mapCountryData(n, nameColumnToPlot="value", mapTitle="", colourPalette=colourPal
#                , addLegend = FALSE, mapRegion = 'Latin America', border="black")
