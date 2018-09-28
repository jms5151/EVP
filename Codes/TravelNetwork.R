##################################################
########### network analysis for travel ##########
################### 5/16/2017 ####################
##################################################

rm(list = ls()) # remove everything stored in environment
library(plyr)
library(GGally)
library(network)
library(sna)
library(ggplot2)

travel <- read.csv("Kenya/Concatenated_Data/TravelRouteCoordinates_2017_05_01.csv", head=T, stringsAsFactors = F)

travelNet <- ddply(travel, .(wheretravel),
                   summarise,
                   chulaimbo = sum(city == "chulaimbo"),
                   msambweni = sum(city == "msambweni"),
                   kisumu = sum(city == "kisumu"),
                   ukunda = sum(city == "ukunda"))

rownames(travelNet) <- travelNet[,1]
travelNet[,1] <- NULL

### https://briatte.github.io/ggnet/
bip = network(travelNet,
              matrix.type = "bipartite",
              ignore.eval = FALSE,
              names.eval = "weights")

col = c("actor" = "#3B9AB2", "event" = "gold")
set.edge.attribute(bip, "lty", ifelse(bip %e% "weights" > 10, 1, 3))
ggnet2(bip, color = "mode", palette = col, edge.lty = "lty", node.alpha = 1/2, max_size = 9) #node.size, label=TRUE

### prevalence data: 9.4% of denv positive individuals traveled >10km prior to testing (23/244)

prevDENV <- read.csv("prev_denv_w_PCR_18_Apr_2017.csv", head=T)

denV <- prevDENV[,c("stanforddenvigg_", "city", "wheretravel", "nightaway", "school")]

denPos <- subset(denV, stanforddenvigg_ == 1)

denPosTravel <- ddply(denPos, .(wheretravel),
                    summarise,
                    chulaimbo = sum(city == "chulaimbo"),
                    msambweni = sum(city == "msambweni"),
                    kisumu = sum(city == "kisumu"),
                    ukunda = sum(city == "ukunda"))

# remove n/a, 0, and NA rows
denPosTravel <- denPosTravel[- grep("n/a", denPosTravel$wheretravel),]
denPosTravel <- subset(denPosTravel, wheretravel != "")
denPosTravel <- subset(denPosTravel, wheretravel != "0")

rownames(denPosTravel) <- denPosTravel[,1]
denPosTravel[,1] <- NULL

prev = network(denPosTravel,
              matrix.type = "bipartite",
              ignore.eval = FALSE,
              names.eval = "weights")

ggnet2(prev, color = "mode", palette = col, node.alpha = 1/2, label=TRUE, color.legend = "City", edge.size="weights") 

## a second way to make a network map: map does not have duplicate city names (like above), but does not indicate edge weights either
DP <- denPos[,2:3]
DP <- DP[- grep("n/a", DP$wheretravel),]
DP <- subset(DP, wheretravel != "")
DP <- subset(DP, wheretravel != "0")
mytable <- ddply(DP,.(city,wheretravel),nrow)
ggnet2(mytable, label=T, color = "red", node.alpha = .7, arrow.size = 9, arrow.gap = 0.02, edge.alpha = 0.25)

### incidence data only has 16 confirmed cases as of April 7, 2017 with 2 of the 16 individuals having traveled prior (12.5%)

# incDENV <- read.csv("inc_denv_w_PCR_ 7_Apr_2017.csv", head=T)
# 
# incDenV <- incDENV[,c("stanforddenvigg_", "city", "wheretravel", "nightaway", "school")]
# 
# incPosDen <- subset(incDenV, stanforddenvigg_ == 1)
# 
# incDenPosTravel <- ddply(incPosDen, .(wheretravel),
#                       summarise,
#                       Chulaimbo = sum(city == "chulaimbo"),
#                       Msambweni = sum(city == "msambweni"),
#                       Kisumu = sum(city == "kisumu"),
#                       Ukunda = sum(city == "ukunda"))
# 
# # remove n/a, 0, and NA rows
# incDenPosTravel <- incDenPosTravel[- grep("n/a", incDenPosTravel$wheretravel),]
# incDenPosTravel <- subset(incDenPosTravel, wheretravel != "")
# incDenPosTravel <- subset(incDenPosTravel, wheretravel != "0")
# 
# rownames(incDenPosTravel) <- incDenPosTravel[,1]
# incDenPosTravel[,1] <- NULL
# 
# inc = network(incDenPosTravel,
#                matrix.type = "bipartite",
#                ignore.eval = FALSE,
#                names.eval = "weights")
# 
# ggnet2(inc, color = "mode", palette = col, node.alpha = 1/2, label=TRUE, color.legend = "City", edge.size="weights")
