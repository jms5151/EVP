#########################################################################
#################### raw vector data, saved 4/21/2017 or 5/2/2017 ###################
#########################################################################
rm(list=ls()) #remove previous variable assignments
library(stringr)
library(plotly)
library(data.table)
library(plyr)
library(reshape2)
library(lubridate)

files <- list.files("VectorData/westVector")

for (i in files){
  fileName <- paste0("VectorData/westVector/", i)
  stringLen <- nchar(i) - 4
  tempName <- paste0(substring(i,1,stringLen), "_west")
  assign(tempName,read.csv(fileName, header=T, stringsAsFactors = F))
}

files <- list.files("VectorData/coastVector")

for (i in files){
  fileName <- paste0("VectorData/coastVector/", i)
  stringLen <- nchar(i) - 4
  tempName <- paste0(substring(i,1,stringLen), "_coast")
  assign(tempName,read.csv(fileName, header=T, stringsAsFactors = F))
}

#################### BG sentinel ###################
BG_Sentinel_data_coast$Date <- as.Date(BG_Sentinel_data_coast$Date, "%m/%d/%Y")
BG_Sentinel_data_coast$Month <- format(BG_Sentinel_data_coast$Date, "%B")
BG_Sentinel_data_coast$Year <- format(BG_Sentinel_data_coast$Date, "%Y")
BG_Sentinel_data_coast$Site <- gsub("Nganja|Milalani|nganja", "Msambweni", BG_Sentinel_data_coast$Site)
BG_Sentinel_data_coast$Site <- gsub("Diani A|Diani B", "Ukunda", BG_Sentinel_data_coast$Site)
BG_Sentinel_data_coast$Species <- gsub("Aedes aegypti|Ae. Aegypti|Aedes sp.|Ae.aegypti", "Aedes_aegypti", BG_Sentinel_data_coast$Species)
# BG_Sentinel_data_coast$Species <- gsub("Aedes |Ae. |Ae.", "Aedes_", BG_Sentinel_data_coast$Species)
# BG_Sentinel_data_coast$Species <- gsub("None|none|Not done", NA, BG_Sentinel_data_coast$Species)
# BG_Sentinel_data_coast$Species <- gsub("An.|An. ", "Anopholes_", BG_Sentinel_data_coast$Species)
# BG_Sentinel_data_coast$Species <- gsub("culex", "Culex", BG_Sentinel_data_coast$Species)
# BG_Sentinel_data_coast$Species <- gsub("Aedes_Aedes_ypti", "Aedes_aegypti", BG_Sentinel_data_coast$Species)
# BG_Sentinel_data_coast$Species <- gsub("Anopholes_ funestus|Anopholes_ Funestus|Anopholes_Funestus", "Anopholes_funestus", BG_Sentinel_data_coast$Species)
# BG_Sentinel_data_coast$Species <- gsub("Anopholes_gambie", "Anopholes_gambiae", BG_Sentinel_data_coast$Species)
setnames(BG_Sentinel_data_coast, c("House.ID", "Domestic.animals", "Site.number"), c("House_ID", "Domestic_animals", "Site_number"))
BGcoast <- BG_Sentinel_data_coast[,c("Date", "Month", "Year", "Survey", "Site", "Site_number", "House_ID", "Species",
                                     "Unfed", "Fed", "Halfgravid", "Gravid", "Male", "Occupants", "Domestic_animals")]

`BioGents-Sentinel_Trap_Mosquito_Sampling_Data_west`$Date <- as.Date(`BioGents-Sentinel_Trap_Mosquito_Sampling_Data_west`$Date..Day, "%B %d, %Y")
`BioGents-Sentinel_Trap_Mosquito_Sampling_Data_west`$Month <- format(`BioGents-Sentinel_Trap_Mosquito_Sampling_Data_west`$Date, "%B")
`BioGents-Sentinel_Trap_Mosquito_Sampling_Data_west`$Year <- format(`BioGents-Sentinel_Trap_Mosquito_Sampling_Data_west`$Date, "%Y")
## the next line is assuming the last 4 digits refers to house
`BioGents-Sentinel_Trap_Mosquito_Sampling_Data_west`$House_ID <- substring(`BioGents-Sentinel_Trap_Mosquito_Sampling_Data_west`$House.Code, 11,14)
Biogents <- `BioGents-Sentinel_Trap_Mosquito_Sampling_Data_west`[,c("Date", "Month", "Year", "Survey", "Site", "SiteNo.", "House_ID",
                                                                    "Aedes.Male", "Aedes.unfed", "Aedes.Blood.fed", "Aedes.Half.gravid",
                                                                    "Aedes..Gravid")]
Biogents$Species <- "Aedes_aegypti"
setnames(Biogents, c( "SiteNo.", "Aedes.Male", "Aedes.unfed", "Aedes.Blood.fed", "Aedes.Half.gravid", "Aedes..Gravid"),
         c("Site_number", "Male", "Unfed", "Fed", "Halfgravid", "Gravid"))
Biogents$Occupants <- as.integer(NA)
Biogents$Domestic_animals <- as.integer(NA)
BGwest <- Biogents[,c("Date", "Month", "Year", "Survey", "Site", "Site_number", "House_ID", "Species",
                      "Unfed", "Fed", "Halfgravid", "Gravid", "Male", "Occupants", "Domestic_animals")]
BG <- rbind(BGcoast, BGwest)
aedesBG <- subset(BG, Species == "Aedes_aegypti")
aedesBG$Female <- aedesBG$Unfed + aedesBG$Fed + aedesBG$Halfgravid + aedesBG$Gravid
write.csv(aedesBG, "Concatenated_Data/vector/BG_aegypti_allSites_2017_05_16.csv", row.names=F)

#################### HLC ###################
HLC_coast$Date <- mdy(HLC_coast$Date)
HLC_coast$Month <- format(HLC_coast$Date, "%B")
HLC_coast$Year <- format(HLC_coast$Date, "%Y")
HLC_coast$Species <- gsub("Ae.aegypti|Ae. Aegypti|Ae. aegypti|Aedes|ae.spp|Ae.spp", "Aedes_aegypti", HLC_coast$Species)
# HLC_coast$Species <- gsub("An.funestus|An. funestus|An. Funestus", "Anopholes_funestus", HLC_coast$Species)
# HLC_coast$Species <- gsub("An.gambiae|AN.gambiae|An. gambie|An.gambie", "Anopholes_gambiae", HLC_coast$Species)
# HLC_coast$Species <- gsub("Not done|None|none", NA, HLC_coast$Species)
# HLC_coast$Species <- gsub("Aedes|ae.spp|Ae.spp", "Aedes_sp.", HLC_coast$Species)
# HLC_coast$Species <- gsub("Ae. Simpsoni|Ae.simpsoni.|ae.simpsoni|Aedes_sp. simpsoni|Ae.simpsoni", "Aedes_simpsoni", HLC_coast$Species)
HLCcoast <- subset(HLC_coast, Species == "Aedes_aegypti")
setnames(HLCcoast, c("House.ID"), c("House_ID"))
hlcCoast <- HLCcoast[,c("Date", "Month", "Year", "Location", "Time", "Survey", "Site", "House_ID",
                        "Species", "Unfed", "Fed", "Halfgravid", "Gravid", "Male")]
hlcCoast$Total_Aedes_aegypti <- as.integer(NA)
hlcCoast$Time <- gsub("[.]", ":", hlcCoast$Time)

HLCwest <- HLC_Adult_Mosquito_Sampling_Data_west[,c(1,2,5,6,8,9,11:27)]
setnames(HLCwest, c( "Aedes.aegypti.4.30.5.30", "Aedes.aegypti.5.30.6.30", "Aedes.aegypti.6.30.7.30", "Aedes.aegypti.7.30.8.30",
                     "Aedes.aegypti.8.30.9.30", "Aedes.aegypti.9.30.10.30", "Aedes.aegypti.10.30.11.30", "Aedes.aegypti.11.30.12.30",
                     "Aedes.aegypti.12.30.1.30", "Aedes.aegypti.1.30.2.30", "Aedes.aegypti.2.30.3.30",  "Aedes.aegypti.3.30.4.30",
                     "Aedes.aegypti.4.30.5.30.1", "Aedes.aegypti.5.30.6.30.1", "Aedes.aegypti.6.30.7.30.1", "Aedes.aegypti.7.30.8.30.1",
                     "Aedes.aegypti.8.30.9.30.1", "Site.No.", "House.No.", "Collection.place"),
         c("4:30-5:30am", "5:30-6:30am", "6:30-7:30am", "7:30-8:30am", "8:30-9:30am", "9:30-10:30am", "10:30-11:30am", "11:30-12:30pm",
           "12:30-1:30pm", "1:30-2:30pm", "2:30-3:30pm", "3:30-4:30pm", "4:30-5:30pm", "5:30-6:30pm", "6:30-7:30pm", "7:30-8:30pm", 
           "8:30-9:30pm", "Site_number", "House_ID", "Location"))
hlcWest <- melt(HLCwest, id.vars = c("Date", "Survey", "Site", "House_ID", "Location"),
                variable.name = "Time", 
                value.name = "Total_Aedes_aegypti")
hlcWest2 <- hlcWest[!is.na(hlcWest$Total_Aedes_aegypti),]
hlcWest2$Date <- as.Date(hlcWest2$Date, "%B %d, %Y")
hlcWest2$Month <- format(hlcWest2$Date, "%B")
hlcWest2$Year <- format(hlcWest2$Date, "%Y")
hlcWest2$Species <- "Aedes_aegypti"
hlcWest2[c("Unfed", "Fed", "Halfgravid", "Gravid", "Male")] <- as.integer(NA)
hlcWest3 <- hlcWest2[,c("Date", "Month", "Year", "Location", "Time", "Survey", "Site", "House_ID",
                        "Species", "Unfed", "Fed", "Halfgravid", "Gravid", "Male", "Total_Aedes_aegypti")]
aedesHLC <- rbind(hlcCoast, hlcWest3)
aedesHLC$Location <- gsub("Indoor", "Inside", aedesHLC$Location)
aedesHLC$Location <- gsub("outdoor|Outdoor|outside", "Outside", aedesHLC$Location)
aedesHLC$House_ID[aedesHLC$House_ID==10463] <- 1001
aedesHLC$House_ID[aedesHLC$House_ID==21213] <- 2003
aedesHLC$House_ID[aedesHLC$House_ID==20945] <- 2001
aedesHLC$House_ID[aedesHLC$House_ID==21294] <- 2002
aedesHLC$House_ID[aedesHLC$House_ID==21305] <- 2004
aedesHLC$Female <- aedesHLC$Unfed + aedesHLC$Fed + aedesHLC$Halfgravid + aedesHLC$Gravid
  
write.csv(aedesHLC, "Concatenated_Data/vector/HLC_aegypti_allSites_2017_05_16.csv", row.names=F)

#################### Ovitrap data ###################
Ovitrapdata_coast$Date <- as.Date(Ovitrapdata_coast$Date, "%m/%d/%Y")
Ovitrapdata_coast$Month <- format(Ovitrapdata_coast$Date, "%B")
Ovitrapdata_coast$Year <- format(Ovitrapdata_coast$Date, "%Y")
Ovitrapdata_coast$Site <- gsub("Nganja|Milalani|nganja", "Msambweni", Ovitrapdata_coast$Site)
Ovitrapdata_coast$Site <- gsub("mwamambi A|mwamambi B|Mwamambi B|Mwamambi A", "Ukunda", Ovitrapdata_coast$Site)
Ovitrapdata_coast$wall.type <- gsub("|[_]", NA, Ovitrapdata_coast$wall.type)
Ovitrapdata_coast$Roof.type <- gsub("|[_]", NA, Ovitrapdata_coast$Roof.type)
Ovitrapdata_coast$Egg.count <- as.numeric(Ovitrapdata_coast$Egg.count)
Ovitrapdata_coast$Species <- gsub("Aedes aegypti|A.aegypti/simpsoni|Ae. Aegypti|Ae. Aegypti/Culex|Ae.aegypti/simpsoni|Ae.aegypti|Aedes", "Aedes_aegypti", Ovitrapdata_coast$Species)
ovitrapCoast <- subset(Ovitrapdata_coast, Species == "Aedes_aegypti")
roomCodes <- read.csv("room_codes.csv", head=T, stringsAsFactors = F)
setnames(ovitrapCoast, c("Date", "Site.Number", "House.ID", "Location", "Room.Place", "wall.type", "Roof.type", "Egg.count"),
         c("Date_collected", "Site_number", "House_ID", "Location", "Room", "Wall_type", "Roof", "Egg_count"))
ovitrapCoast2 <- merge(ovitrapCoast, roomCodes, by="Room", all.x=T)
ovitrapCoast2$Remark <- NULL
ovitrapCoast2$Room <- ovitrapCoast2$Room_description 
ovitrapCoast2$Date_set <- as.Date(NA)
ovitrapCoast2$Duration <- 5
ovitrapCoast2[c("Early_instars", "Late_instars", "Male", "Female", "ovitrapTotal")] <- as.integer(NA)
ovitrapCoast2$Location <- gsub("Outdoor", "Outside", ovitrapCoast2$Location)
ovitrapCoast2$Location <- gsub("Indoor", "Inside", ovitrapCoast2$Location)
ovitrapCoast3 <- ovitrapCoast2[,c("Date_set", "Date_collected", "Duration", "Month", "Year", "Site", "Site_number", "Survey", "Location", "House_ID",
                                 "Wall_type", "Roof", "Room", "Species", "Egg_count", "Early_instars", "Late_instars", "Male", "Female", "ovitrapTotal")]

indoors <- Ovitrap_Sampling_Data_west[,c("Date.set..Day", "Team.Leader", "Date.collected..Day", "Site", "Site.No.", "House.ID", 
                                         "House.type..wall", "Roof", "Room", "Mosquito.type", "Egg.count", "Early.instars",
                                         "Late.instars", "Male", "Female")]
indoors$Location <- "Inside"
outdoors <- Ovitrap_Sampling_Data_west[,c("Date.set..Day", "Team.Leader", "Date.collected..Day", "Site", "Site.No.", "House.ID", 
                                          "House.type..wall", "Roof", "Place", "Mosquito.type.1", "Egg.count.1", "Early.instarts",
                                          "Late.instars.1", "Male.1", "Female.1")]
outdoors$Location <- "Outside"
setnames(outdoors, c("Place", "Mosquito.type.1", "Egg.count.1", "Early.instarts", "Late.instars.1", "Male.1", "Female.1"), 
         c("Room", "Mosquito.type", "Egg.count", "Early.instars", "Late.instars", "Male", "Female"))
ovitrapWest <- rbind(indoors, outdoors)
ovitrapWest$Date_set <- as.Date(ovitrapWest$Date.set..Day, "%B %d, %Y")
ovitrapWest$Date_collected <- as.Date(ovitrapWest$Date.collected..Day, "%B %d, %Y")
ovitrapWest$Month <- format(ovitrapWest$Date_collected, "%B")
ovitrapWest$Year <- format(ovitrapWest$Date_collected, "%Y")
ovitrapWest$Duration <- as.numeric(difftime(ovitrapWest$Date_collected, ovitrapWest$Date_set, units="days"))
ovitrapWest$Survey <- as.integer(NA)
ovitrapWest$House_ID <- substring(ovitrapWest$House.ID, 11,14)
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
ovitrapWest$Room <- trim(ovitrapWest$Room)
ovitrapWest2 <- subset(ovitrapWest, Mosquito.type == "Aedes aegyti")
ovitrapWest2$Species <- "Aedes_aegypti"
setnames(ovitrapWest2, c("Site.No.", "House.type..wall", "Egg.count", "Early.instars", "Late.instars"),
         c("Site_number", "Wall_type", "Egg_count", "Early_instars", "Late_instars"))
ovitrapWest2$ovitrapTotal <- ovitrapWest2$Male + ovitrapWest2$Female
ovitrapWest3 <- ovitrapWest2[,c("Date_set", "Date_collected", "Duration", "Month", "Year", "Site", "Site_number", "Survey", "Location", "House_ID",
                                "Wall_type", "Roof", "Room", "Species", "Egg_count", "Early_instars", "Late_instars", "Male", "Female", "ovitrapTotal")]

aedesOvitrap <- rbind(ovitrapCoast3, ovitrapWest3)
aedesOvitrap$Room <- gsub("Sitting Room|Sitting_room", "Sitting room", aedesOvitrap$Room)
aedesOvitrap$Room <- gsub("Vegetable garden", "Garden", aedesOvitrap$Room)
aedesOvitrap$Total_larvae <- aedesOvitrap$Early_instars + aedesOvitrap$Late_instars
write.csv(aedesOvitrap, "Concatenated_Data/vector/Ovitrap_aegypti_allSites_2017_05_16.csv", row.names=F)

#################### Prokopak data ###################
### WEST data does not indicate species!
inside <- Prokopack_Sampling_Data_west[,c("Date", "Time", "Survey", "Site", "Site.No.", "House.ID", "House.wall", "House.roof",
                                          "No..of.rooms", "Firewood.use.in.the.house", "No..of.sleeper", "Insecticide.sprayed",
                                          "Mosquito.coil.burn", "Bed.net.present", "Eaves.open", "Rooms.with.ceilings", "Aedes.Male",
                                          "Aedes.unfed", "Aedes.Blood.fed", "Aedes.Half.gravid", "Aedes..Gravid")]
inside$Location <- "Inside"
inside$Bushes.around.the.house <- as.numeric(NA)
inside$Tall.grass.around.the.house <- as.numeric(NA)

outside <- Prokopack_Sampling_Data_west[,c("Date", "Time", "Survey", "Site", "Site.No.", "House.ID", "Bushes.around.the.house",
                                           "Tall.grass.around.the.house", "Aedes.Male.1", "Aedes..Unfed", "Aedes.Bloodfed",
                                           "Aedes.Half.gravid.1", "Aedes.gravid")]
outside$Location <- "Outside"
outside[c("House.wall", "House.roof", "No..of.rooms", "Firewood.use.in.the.house", "No..of.sleeper",
          "Insecticide.sprayed", "Mosquito.coil.burn", "Bed.net.present", "Eaves.open", "Rooms.with.ceilings")] <- as.integer(NA)

setnames(outside, c("Aedes.Male.1", "Aedes..Unfed", "Aedes.Bloodfed", "Aedes.Half.gravid.1", "Aedes.gravid"), 
         c("Aedes.Male","Aedes.unfed", "Aedes.Blood.fed", "Aedes.Half.gravid", "Aedes..Gravid"))

outside2 <- outside[,c("Date", "Time", "Survey", "Site", "Site.No.", "House.ID", "House.wall", "House.roof", "No..of.rooms", 
                       "Firewood.use.in.the.house", "No..of.sleeper", "Insecticide.sprayed", "Mosquito.coil.burn", "Bed.net.present", 
                       "Eaves.open", "Rooms.with.ceilings", "Aedes.Male", "Aedes.unfed", "Aedes.Blood.fed", "Aedes.Half.gravid",
                       "Aedes..Gravid", "Location", "Bushes.around.the.house", "Tall.grass.around.the.house")]

prokopak <- rbind(inside, outside)
prokopak$Date <- as.Date(prokopak$Date, "%B %d, %Y")
prokopak$Month <- format(prokopak$Date, "%B")
prokopak$Year <- format(prokopak$Date, "%Y")
prokopak$House_ID <- substring(prokopak$House.ID, 11,14)

setnames(prokopak, c("Site.No.", "House.wall", "House.roof", "No..of.rooms", "Firewood.use.in.the.house", "No..of.sleeper",
                     "Insecticide.sprayed", "Mosquito.coil.burn", "Bed.net.present", "Eaves.open", "Rooms.with.ceilings",
                     "Aedes.Male", "Aedes.unfed", "Aedes.Blood.fed", "Aedes.Half.gravid", "Aedes..Gravid",
                     "Bushes.around.the.house", "Tall.grass.around.the.house"),
         c("Site_number", "Wall_type", "Roof", "Number_of_rooms", "Firewood_use_in_house", "Number_of_sleepers",
           "Insecticide_sprayed", "Mosquito_coil_burn", "Bed_net_present", "Eaves_open", "Ceiling_present",
           "Male", "Unfed", "Fed", "Halfgravid", "Gravid", "Bushes_around_house", "Tall_grass_around_house"))
prokopak$Species <- "Aedes_aegypti"
prokopak$Total_mosquitoes <- as.integer(NA)
prokWest <- prokopak[,c("Date", "Month", "Year", "Site", "Site_number", "Survey", "House_ID", "Location", "Time",
                        "Wall_type", "Roof", "Number_of_rooms", "Firewood_use_in_house", "Number_of_sleepers",
                        "Insecticide_sprayed", "Mosquito_coil_burn", "Bed_net_present", "Eaves_open", "Ceiling_present",
                        "Bushes_around_house", "Tall_grass_around_house", "Species", "Male", "Unfed", "Fed", "Halfgravid",
                        "Gravid", "Total_mosquitoes")]

ProkopackF_coast$Date <- mdy(ProkopackF_coast$Date)
ProkopackF_coast$Month <- format(ProkopackF_coast$Date, "%B")
ProkopackF_coast$Year <- format(ProkopackF_coast$Date, "%Y")
ProkopackF_coast$Site <- gsub("ukunda", "Ukunda", ProkopackF_coast$Site2)
ProkopackF_coast$Site <- gsub("milalani|nganja|Nganja|Milalani", "Msambweni", ProkopackF_coast$Site)
ProkopackF_coast$Location[ProkopackF_coast$Location == 1] <- "Inside"
ProkopackF_coast$Location[ProkopackF_coast$Location == 2] <- "Outside"
ProkopackF_coast$Species <- gsub("Ae.aegypti|ae.aegypti|Ae. Aegypti|Aedes aegypti|Aedes sp|aedes aedes|aedes aegypti|Aedes spp| Aedea spp", "Aedes_aegypti", ProkopackF_coast$Species)
#ProkopackF_coast$Genus <- gsub("Aedes_aegypti|Ae.simpsoni|Ae. Simp|Aedes sp|Aedes simpsoni|aedes aedes|Aedes spp| Aedea spp", "Aedes", ProkopackF_coast$Species)
prokCoast <- subset(ProkopackF_coast, Species == "Aedes_aegypti")
prokCoast[prokCoast$Roof == 0] <- NA
prokCoast[prokCoast$Roof == 3] <- NA
prokCoast[prokCoast$Roof == 4] <- NA
prokCoast[prokCoast$Roof == 5] <- NA

setnames(prokCoast, c("Survey2", "SiteNo", "HouseID", "Housewall", "Houseroof", "Noofrooms", "Firewooduse", "peopleslept",
                      "Insecticidesprayed", "Mosquitocoil", "Bednetpresent", "Eavesopen", "Ceilingpresent",
                      "Bushesaround", "Tallgrassaround", "total.mosq.count"),
         c("Survey", "Site_number", "House_ID", "Wall_type", "Roof", "Number_of_rooms", "Firewood_use_in_house", "Number_of_sleepers",
           "Insecticide_sprayed", "Mosquito_coil_burn", "Bed_net_present", "Eaves_open", "Ceiling_present",
           "Bushes_around_house", "Tall_grass_around_house", "Total_mosquitoes"))

prokoCoast <- prokCoast[,c("Date", "Month", "Year", "Site", "Site_number", "Survey", "House_ID", "Location", "Time",
                           "Wall_type", "Roof", "Number_of_rooms", "Firewood_use_in_house", "Number_of_sleepers",
                           "Insecticide_sprayed", "Mosquito_coil_burn", "Bed_net_present", "Eaves_open", "Ceiling_present",
                           "Bushes_around_house", "Tall_grass_around_house", "Species", "Male", "Unfed", "Fed", "Halfgravid",
                           "Gravid", "Total_mosquitoes")]

prokoAll <- rbind(prokWest, prokoCoast)
prokoAll$Time <- gsub(" ", "", prokoAll$Time)
prokoAll$Time <- gsub("11:AM", "11:00AM", prokoAll$Time)
prokoAll$Time <- gsub("pm", "PM", prokoAll$Time)
prokoAll$Time <- gsub("am", "AM", prokoAll$Time)
prokoAll$Time <- gsub("[.]|[;]", ":", prokoAll$Time)
prokoAll$Female <- prokoAll$Unfed + prokoAll$Fed + prokoAll$Halfgravid + prokoAll$Gravid
write.csv(prokoAll, "Concatenated_Data/vector/Prokopak_Aedes_allsites_2017_05_16.csv", row.names=F)

#################### Larval sampling ###################
Larval_Sampling_Data_west$Date <- as.Date(Larval_Sampling_Data_west$Date, "%B %d, %Y")
Larval_Sampling_Data_west$Month <- format(Larval_Sampling_Data_west$Date, "%B")
Larval_Sampling_Data_west$Year <- format(Larval_Sampling_Data_west$Date, "%Y")
Larval_Sampling_Data_west$Time <- gsub(" ", "", Larval_Sampling_Data_west$Time)
Larval_Sampling_Data_west$Time <- gsub("pm", "PM", Larval_Sampling_Data_west$Time)
Larval_Sampling_Data_west$Time <- gsub("am", "AM", Larval_Sampling_Data_west$Time)
Larval_Sampling_Data_west$House_ID <- substring(Larval_Sampling_Data_west$House.code, 11,14)
Larval_Sampling_Data_west$IN.OUT.DOORS <- gsub("indoors|Indoors", "Inside", Larval_Sampling_Data_west$IN.OUT.DOORS)
Larval_Sampling_Data_west$IN.OUT.DOORS <- gsub("outdoors|Outdoors", "Outside", Larval_Sampling_Data_west$IN.OUT.DOORS)
Larval_Sampling_Data_west$IN.OUT.DOORS <- gsub(" ", NA, Larval_Sampling_Data_west$IN.OUT.DOORS)
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
Larval_Sampling_Data_west$Room <- trim(Larval_Sampling_Data_west$Room.Place)
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
Larval_Sampling_Data_west$Room <- firstup(Larval_Sampling_Data_west$Room)
Larval_Sampling_Data_west$Habitat_type <- tolower(Larval_Sampling_Data_west$Habitat.type)
Larval_Sampling_Data_west$Species <- gsub(" ", "_", Larval_Sampling_Data_west$Species)
larvalWest <- subset(Larval_Sampling_Data_west, Species == "Aedes_aegypti")
setnames(larvalWest, c("Site.code",  "No.of.sleepers", "IN.OUT.DOORS", "Habitat.count", "Early.instars",  "Late.instars"),
         c("Site_number", "Number_of_sleepers", "Location", "Habitat_count", "Early_instars", "Late_instars"))

larvalWest2 <- larvalWest[,c("Date", "Time", "Month", "Year", "Site", "Site_number", "Location", "House_ID", "Room", "Number_of_sleepers", 
                             "Habitat_type", "Habitat_count", "Species", "Early_instars", "Late_instars", "Pupae")]

LarvalF_coast$Date <- as.Date(LarvalF_coast$Date, "%m/%d/%Y")
LarvalF_coast$Month <- format(LarvalF_coast$Date, "%B")
LarvalF_coast$Year <- format(LarvalF_coast$Date, "%Y")
LarvalF_coast$Location <- gsub("1ndoor|indoor|Indoor", "Inside", LarvalF_coast$Location)
LarvalF_coast$Location <- gsub("outdoor|Outdoor", "Outside", LarvalF_coast$Location)
LarvalF_coast$Site <- gsub("Diani A|Diani A |Diani B|DianiA|DianiB", "Ukunda", LarvalF_coast$Site)
LarvalF_coast$Site <- gsub("Milalani|Nganja", "Msambweni", LarvalF_coast$Site)

LC <- ddply(LarvalF_coast, .(Date, Time, Month, Year, Location, Site, Site.code, Survey, HouseID, Room, Containertype, SpeciesL),
                                  summarise,
                                  Habitat_count = length(Containertype),
                                  Earlyinstars = sum(na.omit(Earlyinstars)),
                                  Lateinstars = sum(na.omit(Lateinstars)),
                                  Pupae = sum(na.omit(Pupae)))

roomCodes <- read.csv("room_codes.csv", head=T, stringsAsFactors = F)
LarvalCoast <- merge(LC, roomCodes, by="Room", all.x=T)
LarvalCoast$Room <- LarvalCoast$Room_description
LarvalCoast$Room_description <- NULL

containerCodes <- read.csv("container_codes.csv", head=T, stringsAsFactors = F)
LarvalCoast2 <- merge(LarvalCoast, containerCodes, by="Containertype", all.x=T)
LarvalCoast2$Number_of_sleepers <- as.integer(NA)

setnames(LarvalCoast2, c("Site.code",  "HouseID", "Earlyinstars",  "Lateinstars", "SpeciesL"),
         c("Site_number", "House_ID", "Early_instars", "Late_instars", "Species"))

larvalCoast3 <- LarvalCoast2[,c("Date", "Time", "Month", "Year", "Site", "Site_number", "Location", "House_ID", "Room", 
                                "Number_of_sleepers", "Habitat_type", "Habitat_count", "Species", "Early_instars", "Late_instars", "Pupae")]

larvae <- rbind(larvalWest2, larvalCoast3) 
larvae$Time <- gsub(" ", "", larvae$Time)
larvae$Time <- gsub("pm", "PM", larvae$Time)
larvae$Time <- gsub("am", "AM", larvae$Time)
larvae$Time <- gsub("[.]|[;]", ":", larvae$Time)
larvae$Species <- gsub("Aedes$", "Aedes_aegypti", larvae$Species) # this line shouldn't be needed once data is updated
aedesLarvae <- subset(larvae, Species == "Aedes_aegypti")
aedesLarvae$Total_larvae <- aedesLarvae$Early_instars + aedesLarvae$Late_instars

write.csv(aedesLarvae, "Concatenated_Data/vector/larvae_aedes_allsites_2017_05_16.csv", row.names=F)

#######################################################################
####################### old larvae code, includes all mosquito columns
#######################################################################
# Larval_Sampling_Data_west$Date <- as.Date(Larval_Sampling_Data_west$Date, "%B %d, %Y")
# Larval_Sampling_Data_west$Month <- format(Larval_Sampling_Data_west$Date, "%B")
# Larval_Sampling_Data_west$Year <- format(Larval_Sampling_Data_west$Date, "%Y")
# Larval_Sampling_Data_west$Time <- gsub(" ", "", Larval_Sampling_Data_west$Time)
# Larval_Sampling_Data_west$Time <- gsub("pm", "PM", Larval_Sampling_Data_west$Time)
# Larval_Sampling_Data_west$Time <- gsub("am", "AM", Larval_Sampling_Data_west$Time)
# Larval_Sampling_Data_west$House_ID <- substring(Larval_Sampling_Data_west$House.code, 11,14)
# Larval_Sampling_Data_west$IN.OUT.DOORS <- gsub("indoors|Indoors", "Inside", Larval_Sampling_Data_west$IN.OUT.DOORS)
# Larval_Sampling_Data_west$IN.OUT.DOORS <- gsub("outdoors|Outdoors", "Outside", Larval_Sampling_Data_west$IN.OUT.DOORS)
# Larval_Sampling_Data_west$IN.OUT.DOORS <- gsub(" ", NA, Larval_Sampling_Data_west$IN.OUT.DOORS)
# # returns string w/o leading or trailing whitespace
# trim <- function (x) gsub("^\\s+|\\s+$", "", x)
# Larval_Sampling_Data_west$Room <- trim(Larval_Sampling_Data_west$Room.Place)
# firstup <- function(x) {
#   substr(x, 1, 1) <- toupper(substr(x, 1, 1))
#   x
# }
# Larval_Sampling_Data_west$Room <- firstup(Larval_Sampling_Data_west$Room)
# Larval_Sampling_Data_west$Habitat_type <- tolower(Larval_Sampling_Data_west$Habitat.type)
# Larval_Sampling_Data_west$Species <- gsub(" ", "_", Larval_Sampling_Data_west$Species)
# larvalWest <- subset(Larval_Sampling_Data_west, Species == "Aedes_aegypti")
# setnames(larvalWest, c("Site.code",  "No.of.sleepers", "IN.OUT.DOORS", "Habitat.count", "Early.instars",  "Late.instars"),
#          c("Site_number", "Number_of_sleepers", "Location", "Habitat_count", "Early_instars", "Late_instars"))
# larvalWest[c("Larvaelab", "MaleAL", "FemaleAL", "Noeclose_L", "Pupaelab", "MaleAP", "FemaleAP", "Noeclose_P")] <- as.integer(NA)
# larvalWest[c("SpeciesA", "SpeciesAP", "SpeciesL", "Species", "Container_type")] <- as.character(NA)
# 
# larvalWest2 <- larvalWest[,c("Date", "Time", "Month", "Year", "Site", "Site_number", "Location", "House_ID", "Room", "Number_of_sleepers", 
#                              "Habitat_type", "Habitat_count", "Species", "Early_instars", "Late_instars", "Pupae",
#                              "Larvaelab", "SpeciesA", "MaleAL", "FemaleAL", "Noeclose_L", "Pupaelab",     
#                              "SpeciesAP", "MaleAP", "FemaleAP", "Noeclose_P")]
# 
# LarvalF_coast$Date <- as.Date(LarvalF_coast$Date, "%m/%d/%Y")
# LarvalF_coast$Month <- format(LarvalF_coast$Date, "%B")
# LarvalF_coast$Year <- format(LarvalF_coast$Date, "%Y")
# LarvalF_coast$Location <- gsub("1ndoor|indoor|Indoor", "Inside", LarvalF_coast$Location)
# LarvalF_coast$Location <- gsub("outdoor|Outdoor", "Outside", LarvalF_coast$Location)
# LarvalF_coast$Site <- gsub("Diani A|Diani A |Diani B|DianiA|DianiB", "Ukunda", LarvalF_coast$Site)
# LarvalF_coast$Site <- gsub("Milalani|Nganja", "Msambweni", LarvalF_coast$Site)
# 
# roomCodes <- read.csv("room_codes.csv", head=T, stringsAsFactors = F)
# LarvalCoast <- merge(LarvalF_coast, roomCodes, by="Room", all.x=T)
# LarvalCoast$Room <- LarvalCoast$Room_description
# LarvalCoast$Room_description <- NULL
# LarvalCoast[,c("Larvaelab", "MaleAL", "FemaleAL", "Noeclose_L", "Pupaelab", "MaleAP", "FemaleAP", "Noeclose_P", "Containertype")] = apply(LarvalCoast[,c("Larvaelab", 
#                                                                                                                                                          "MaleAL", "FemaleAL", "Noeclose_L", "Pupaelab", "MaleAP", "FemaleAP", "Noeclose_P", "Containertype")], 2, function(x) as.integer(x))
# 
# 
# containerCodes <- read.csv("container_codes.csv", head=T, stringsAsFactors = F)
# LarvalCoast2 <- merge(LarvalCoast, containerCodes, by="Containertype", all.x=T)
# LarvalCoast2[c("Habitat_count", "Number_of_sleepers")] <- as.integer(NA)
# LarvalCoast2[c("Habitat_type", "Species")] <- as.character(NA)
# 
# setnames(LarvalCoast2, c("Site.code",  "HouseID", "Containersize", "Earlyinstars",  "Lateinstars"),
#          c("Site_number", "House_ID", "Container_size", "Early_instars", "Late_instars"))
# 
# larvalCoast3 <- LarvalCoast2[,c("Date", "Time", "Month", "Year", "Site", "Site_number", "Location", "House_ID", "Room", "Number_of_sleepers", 
#                                 "Habitat_type", "Habitat_count", "Species", "Early_instars", "Late_instars", "Pupae",
#                                 "Larvaelab", "SpeciesA", "MaleAL", "FemaleAL", "Noeclose_L", "Pupaelab",     
#                                 "SpeciesAP", "MaleAP", "FemaleAP", "Noeclose_P")]
# 
# 
# larvae <- rbind(larvalWest2, larvalCoast3) 
# larvae$Time <- gsub(" ", "", larvae$Time)
# larvae$Time <- gsub("pm", "PM", larvae$Time)
# larvae$Time <- gsub("am", "AM", larvae$Time)
# larvae$Time <- gsub("[.]|[;]", ":", larvae$Time)
# larvae$SpeciesA <- gsub("Aedes$", "Aedes_aegypti", larvae$SpeciesA)
# larvae$SpeciesAP <- gsub("Aedes$|aedes$", "Aedes_aegypti", larvae$SpeciesAP)
# # larvae$SpeciesA <- gsub("Ae.simp|Ae.simpsoni|Aedes simp", "Aedes_simpsoni", larvae$SpeciesAP)
# # larvae$SpeciesAP <- gsub("Ae.simp|Ae.simpsoni", "Aedes_simpsoni", larvae$SpeciesAP)
# 
# write.csv(larvae, "Concatenated_Data/vector/larvae_Aedes_allsites_2017_05_09.csv", row.names=F)

#####################################################################
############ vector summary data, saved 4/21/2017 ###################
#####################################################################
files <- list.files("VectorData/monthlySummaries")

for (i in files){
  fileName <- paste0("VectorData/monthlySummaries/", i)
  stringLen <- nchar(i) - 4
  tempName <- paste0(substring(i,1,stringLen))
  assign(tempName,read.csv(fileName, header=T))
}

mon <- read.csv("VectorData/MonAbr.csv", head=T)

######### ovitrap data ################
ovitrap <- rbind(OvitrapMonthlySummaries_ch[,c("Site", "Date", "Aedes.aegypti..Indoor", "Aedes.aegypti..Outdoor")], 
                 OvitrapMonthlySummaries_ki[,c("Site", "Date", "Aedes.aegypti..Indoor", "Aedes.aegypti..Outdoor")])
ovitrap <- rbind(ovitrap, OvitrapMonthlySummaries_ms[,c("Site", "Date", "Aedes.aegypti..Indoor", "Aedes.aegypti..Outdoor")])
ovitrap <- rbind(ovitrap, OvitrapMonthlySummaries_uk[,c("Site", "Date", "Aedes.aegypti..Indoor", "Aedes.aegypti..Outdoor")])
ovitrap$oviTotal <- ovitrap$Aedes.aegypti..Indoor+ovitrap$Aedes.aegypti..Outdoor
ovitrap2 <- ddply(ovitrap, .(Date, Site),
              summarise,
              oviTotal = sum(oviTotal))
ovitrap3 <- merge(mon, ovitrap2, by=c("Date", "Site"), all=T)

######### hlc data ################
### hlc chulaimbo and kisumu dates include h:m:s, need to remove manually for following code to work ###
hlc <- rbind(LandingCatchesMonthlySummaries_ch[,c("date", "site", "Aedes.spp..Total")], LandingCatchesMonthlySummaries_ki[,c("date", "site", "Aedes.spp..Total")])
setnames(hlc, old = c("date", "site", "Aedes.spp..Total"), new = c("Date", "Site", "hlcTotal"))
hlc$Date <- as.Date(hlc$Date, "%m/%d/%Y")
hlc$Date <- format(hlc$Date, "%b %Y")
hlc2 <- rbind(LandingCatchesMonthlySummaries_ms[,c("Date", "Site", "aedes.indoor", "aedes.outdoor")], LandingCatchesMonthlySummaries_uk[,c("Date", "Site", "aedes.indoor", "aedes.outdoor")])
hlc2$hlcTotal <- hlc2$aedes.indoor+hlc2$aedes.outdoor 
hlc2[,c("aedes.indoor",  "aedes.outdoor")] <- NULL
hlc3 <- rbind(hlc, hlc2)
hlc4 <- ddply(hlc3, .(Date, Site),
      summarise,
      hlcTotal = sum(hlcTotal))
hlc5 <- merge(mon, hlc4, by=c("Date", "Site"), all=T)

######### pupae data ################
pupae <- rbind(PupaeMonthlySummaries_ch[,c("Site", "Date", "Aedes.spp")], PupaeMonthlySummaries_ki[,c("Site", "Date", "Aedes.spp")]) 
pupae <- rbind(pupae, PupaeMonthlySummaries_ms[,c("Site", "Date", "Aedes.spp")])
pupae <- rbind(pupae, PupaeMonthlySummaries_uk[,c("Site", "Date", "Aedes.spp")])
pupae2 <- ddply(pupae, .(Date, Site),
              summarise,
              pupaeTotal = sum(Aedes.spp))
pupae3 <- merge(mon, pupae2, by=c("Date", "Site"), all=T)

######### trap data ################
trap <- rbind(SentinelTrapMonthlySummaries_ch[,c("Site", "Date", "Aedes.spp")], SentinelTrapMonthlySummaries_ki[,c("Site", "Date", "Aedes.spp")])
trap2 <- rbind(SentinelTrapMonthlySummaries_ms[,c("Site", "Date", "aedes.spp")], SentinelTrapMonthlySummaries_uk[,c("Site", "Date", "aedes.spp")])
colnames(trap2)[3] <- "Aedes.spp"
trap3 <- rbind(trap, trap2)
trap4 <- ddply(trap3, .(Date, Site),
                summarise,
                trapTotal = sum(Aedes.spp))
trap5 <- merge(mon, trap4, by=c("Date", "Site"), all=T)

######### larval data ################
larvae <- rbind(LarvalMonthlySummaries_ch[,c("Site", "Date", "Aedes.aegypti..Indoor", "Aedes.aegypti..Outdoor")], LarvalMonthlySummaries_ki[,c("Site", "Date", "Aedes.aegypti..Indoor", "Aedes.aegypti..Outdoor")])
larvae <- rbind(larvae, LarvalMonthlySummaries_ms[,c("Site", "Date", "Aedes.aegypti..Indoor", "Aedes.aegypti..Outdoor")])
larvae <- rbind(larvae, LarvalMonthlySummaries_uk[,c("Site", "Date", "Aedes.aegypti..Indoor", "Aedes.aegypti..Outdoor")])
larvae$larvaeTotal <- larvae$Aedes.aegypti..Indoor + larvae$Aedes.aegypti..Outdoor
larvae$Site <- revalue(larvae$Site, c("nganja"="Ukunda", "mwamambi A"="Ukunda", "mwamambi B"="Ukunda", "Mwamambi A"="Ukunda", "Mwamambi B"="Ukunda"))
larvae2 <- ddply(larvae, .(Date, Site),
                 summarise,
                 larvalTotal = sum(larvaeTotal))
larvae3 <- merge(mon, larvae2, by=c("Date", "Site"), all=T)
  
######### prokopak data ################
colnames(ProkopackMonthlySummaries_ch)[1] <- "Date"
ProkopackMonthlySummaries_ch$Site <- "Chulaimbo"
colnames(ProkopackMonthlySummaries_ki)[1] <- "Date"
ProkopackMonthlySummaries_ki$Site <- "Kisumu"
colnames(ProkopackMonthlySummaries_ms)[1] <- "Date"
ProkopackMonthlySummaries_ms$Site <- "Msambweni"
colnames(ProkopackMonthlySummaries_uk)[1] <- "Date"
ProkopackMonthlySummaries_uk$Site <- "Ukunda"
prokopak <- rbind(ProkopackMonthlySummaries_ch, ProkopackMonthlySummaries_ki)
prokopak <- rbind(prokopak, ProkopackMonthlySummaries_ms)
prokopak <- rbind(prokopak, ProkopackMonthlySummaries_uk)
prokopak$prokopakTotal <- prokopak$Ttl_Aedes.spp.Indoor + prokopak$ttl_Aedes_spp_Outdoor  
prokopak2 <- prokopak[,c("Date", "Site", "prokopakTotal")]
prokopak3 <- merge(mon,prokopak2, by=c("Date", "Site"), all=T)

### merge all data ####
vector <- merge(ovitrap3, hlc5, by=c("Site", "DateNum", "Date"))
vector <- merge(vector, pupae3, by=c("Site", "DateNum", "Date"))
vector <- merge(vector, trap5, by=c("Site", "DateNum", "Date"))
vector <- merge(vector, larvae3, by=c("Site", "DateNum", "Date"))
vector <- merge(vector, prokopak3, by=c("Site", "DateNum", "Date"))
vector$DateNum <- as.Date(vector$DateNum, "%m/%d/%Y")
write.csv(vector, "monthlyVectorDataConcat.csv", row.names=F)

plot_ly(vector, x = ~DateNum, y = ~oviTotal, type = 'bar', name = 'ovitrap')%>%
  add_trace(y = ~prokopakTotal, name = 'prokopak') %>%
  add_trace(y = ~hlcTotal, name = 'hlc') %>%
  add_trace(y = ~trapTotal, name = 'trap') %>%
  add_trace(y = ~pupaeTotal, name = 'pupae') %>%
  add_trace(y = ~larvalTotal, name = 'larvae') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'relative') # barmode='group'

plot_ly(vector, x = ~DateNum, y = ~oviTotal, type = 'bar', name = 'ovitrap')
plot_ly(vector, x = ~DateNum, y = ~prokopakTotal, type = 'bar')
plot_ly(vector, x = ~DateNum, y = ~hlcTotal, type = 'bar')
plot_ly(vector, x = ~DateNum, y = ~trapTotal, type = 'bar')
plot_ly(vector, x = ~DateNum, y = ~pupaeTotal, type = 'bar')
plot_ly(vector, x = ~DateNum, y = ~larvalTotal, type = 'bar')


plot_ly(vector, x = ~DateNum, y = ~trapTotal, type = 'bar', name = 'trap')%>%
  add_trace(y = ~pupaeTotal, name = 'pupae') %>%
  add_trace(y = ~larvalTotal, name = 'larvae') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group')

plot_ly(vector, x = ~DateNum, y = ~hlcTotal, type = 'bar', name = 'hlc')

# -------------------- Concatenate data by dates
rm(list=ls()) #remove previous variable assignments
library(zoo)
library(plotly)
library(plyr)

files <- list.files("Concatenated_Data/vector/")

for (i in files){
  fileName <- paste0("Concatenated_Data/vector/", i)
  stringLen <- nchar(i) - 4
  tempName <- paste0(substring(i,1,stringLen))
  assign(tempName,read.csv(fileName, header=T))
}

BG <- ddply(BG_aegypti_allSites_2017_05_16, .(Site, Date),
            summarise,
            BG_female = sum(na.omit(Female)),
            BG_total = sum(na.omit(Female)) + sum(na.omit(Male)))  
BG$Date <- as.Date(BG$Date, "%Y-%m-%d")

HLC <- ddply(HLC_aegypti_allSites_2017_05_16, .(Site, Date),
             summarise,
             HLC_female = sum(na.omit(Female)),
             HLC_total = sum(na.omit(Female)) + sum(na.omit(Total_Aedes_aegypti)))  
HLC$Date <- as.Date(HLC$Date, "%Y-%m-%d")

larvae <- ddply(larvae_aedes_allsites_2017_05_16, .(Site, Date),
                summarise,
                larvae = sum(na.omit(Total_larvae)),
                pupae = sum(na.omit(Pupae)))  
larvae$Date <- as.Date(larvae$Date, "%Y-%m-%d")

Ovitrap <- ddply(Ovitrap_aegypti_allSites_2017_05_16, .(Site, Date_collected),
                 summarise,
                 Ovi_egg_count = sum(na.omit(Egg_count)),
                 Ovi_female = sum(na.omit(Female)))  
Ovitrap$Date <- as.Date(Ovitrap$Date, "%Y-%m-%d")

prokopak <- ddply(Prokopak_Aedes_allsites_2017_05_16, .(Site, Date),
                  summarise,
                  Prok_female = sum(Female),
                  Prok_total = sum(na.omit(Female)) + sum(na.omit(Total_mosquitoes)))  
prokopak$Date <- as.Date(prokopak$Date, "%Y-%m-%d")

vec <- merge(prokopak, Ovitrap, by=c("Site", "Date"), all=T)
vec <- merge(vec, larvae, by=c("Site", "Date"), all=T)
vec <- merge(vec, HLC, by=c("Site", "Date"), all=T)
vec <- merge(vec, BG, by=c("Site", "Date"), all=T)
# vec$Date <- as.yearmon(paste(vector$Year, vector$Month, sep = "-"), "%Y-%B")
# vec$Date <- format(vector$Date, "%Y-%m")
write.csv(vec, "C:/Users/Jamie/Box Sync/DENV/Concatenated_Data/VectorDataConcat_Date_2017_06_14.csv", row.names=F)
