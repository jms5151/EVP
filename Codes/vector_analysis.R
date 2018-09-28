# analyze vector data ------------------------------
rm(list=ls()) #remove previous variable assignments
rm(list= ls()[!(ls() %in% c('clim_vec_backup','redcap_clim_vec'))])

library(car)
library(MASS)

# get data -----------------------------------------------------------------
# source("Codes/REDCap_extract_climate_and_vector_data.R")
bg <- read.csv("Concatenated_Data/vector/redcap_bg.csv", head=T) 
hlc_sex <- read.csv("Concatenated_Data/vector/redcap_hlc_sex.csv", head=T) 
hlc_time <- read.csv("Concatenated_Data/vector/redcap_hlc_time.csv", head=T) 
larvae <- read.csv("Concatenated_Data/vector/redcap_larvae.csv", head=T) 
ovitrap <- read.csv("Concatenated_Data/vector/redcap_ovitrap.csv", head=T) 
prokopack <- read.csv("Concatenated_Data/vector/redcap_prokopack.csv", head=T) 

# subset data and format dates ---------------------------------------------
trapDF <- list(bg, hlc_sex, hlc_time, prokopack, larvae, ovitrap)
completeColNames <- c("bg_complete", "hlc_complete", "hlc_complete", "prokopack_complete", "larva_complete", "ovitrap_complete")
mosqColNames <- c(rep("mosquitoes", 4), "early_instars_larva", "egg_count_ovitrap")
dfNames <- c("bg.sub", "hlc.s.sub", "hlc.t.sub", "prokopack.sub", "larvae.sub", "ovitrap.sub")
speciesNames <- c(rep("aedes_aegypti", 4), rep("Ae. aegypti", 2))

for (i in 1:6){
  tempSub <- trapDF[[i]] 
  tempSub <- subset(tempSub, tempSub[,completeColNames[i]]==2)
  tempSubDF <- tempSub[!is.na(tempSub[,mosqColNames[i]]),]
  tempSubDF$date_collected <- as.Date(tempSubDF$date_collected, "%Y-%m-%d")
  tempSubDF$month <-as.numeric(format(tempSubDF$date_collected, "%m"))
  tempSubDF$year <-as.numeric(format(tempSubDF$date_collected, "%Y"))
  assign(dfNames[i],tempSubDF)
  aeSubDF <- subset(tempSubDF, species==speciesNames[i])
  aeDFName <- paste0(dfNames[i], ".ae")
  assign(aeDFName,aeSubDF)
  aeSubDFpos <- subset(aeSubDF, aeSubDF[,mosqColNames[i]]>0)
  aePosDFName <- paste0(aeDFName, ".pos")
  assign(aePosDFName,aeSubDFpos)
}

# correlation plots ---------------------------------------------------------
# source correlation function
source("C:/Users/Jamie/Box Sync/R_functions/correlation_pairs_fn.R")

mosquitoDFs <- list(bg.sub, bg.sub.ae, bg.sub.ae.pos, hlc.s.sub, hlc.s.sub.ae, hlc.s.sub.ae.pos, hlc.t.sub, hlc.t.sub.ae, hlc.t.sub.ae.pos, prokopack.sub, prokopack.sub.ae, prokopack.sub.ae.pos, larvae.sub, larvae.sub.ae, larvae.sub.ae.pos, ovitrap.sub, ovitrap.sub.ae, ovitrap.sub.ae.pos)
trapNames <- c("bg_all", "bg_aegypti", "bg_aegypti_pos", "hlc_sex_all", "hlc_sex_aegypti", "hlc_sex_aegypti_pos", "hlc_time_all", "hlc_time_aegypti", "hlc_time_aegypti_pos", "prokopack_all", "prokopack_aegypti", "prokopack_aegypti", "larvae_all", "larvae_aegypti", "larvae_aegypti_pos", "ovitrap_all", "ovitrap_aegypti", "ovitrap_aegypti_pos")

for (j in c(1:18)){
  tempDF <- as.data.frame(mosquitoDFs[j])
  subType <- substr(trapNames[j], nchar(trapNames[j]), nchar(trapNames[j]))
  if (subType == "l"){
    tempDF.sub <- tempDF[, grepl("house|time|day|occupants|team|mosquitoes|instars|pupae|egg|species|sex|fed|site|village|month|year|collection_place_hlc|collector|room|habitat_size|habitat_type_larva|place", names(tempDF) ) ]
  } else {
    tempDF.sub <- tempDF[, grepl("house|time|day|occupants|team|mosquitoes|instars|pupae|egg|sex|fed|site|village|month|year|collection_place_hlc|collector|room|habitat_size|habitat_type_larva|place", names(tempDF) ) ]
  }
  fileName <- paste0("Figures/vectors/surveys/corr_plot_", trapNames[j], ".tiff")
  title <- paste0(trapNames[j], " correlation matrix")
  tiff(file = fileName, width = 1234, height = 842)
  pairs(tempDF.sub, lower.panel=panel.smooth, upper.panel=panel.cor,diag.panel=panel.hist, main=title)
  dev.off()
}

# distributions plots ---------------------------------------------
ae.traps <- list(bg.sub.ae, bg.sub.ae.pos, hlc.s.sub.ae, hlc.s.sub.ae.pos, hlc.t.sub.ae, hlc.t.sub.ae.pos, prokopack.sub.ae, prokopack.sub.ae.pos, larvae.sub.ae, larvae.sub.ae.pos, larvae.sub.ae, larvae.sub.ae.pos, larvae.sub.ae, larvae.sub.ae.pos, ovitrap.sub.ae, ovitrap.sub.ae.pos)
mosqVar <- c(rep("mosquitoes",8), rep("early_instars_larva", 2), rep("late_instars_larva", 2), rep("pupae_larva", 2), rep("egg_count_ovitrap", 2))
mozzyNames <- c("Mosquitoes", "logMosquitoes", "inverseMosquitoes")
trapName <- c("bg.sub.ae", "bg.sub.ae.pos", "hlc.s.sub.ae", "hlc.s.sub.ae.pos", "hlc.t.sub.ae", "hlc.t.sub.ae.pos", "prokopack.sub.ae", "prokopack.sub.ae.pos", "larvae.sub.ae", "larvae.sub.ae.pos", "larvae.sub.ae",  "larvae.sub.ae.pos", "larvae.sub.ae",  "larvae.sub.ae.pos", "ovitrap.sub.ae", "ovitrap.sub.ae.pos")

for (k in 1:length(trapName)){
  qqDF <- as.data.frame(ae.traps[k])
  mosquitoes <- qqDF[,mosqVar[k]]
  mosquitoes.log <- log(mosquitoes+1)
  mosquitoes.inv <- 1/(mosquitoes+1)
  mozzies <- list(mosquitoes, mosquitoes.log, mosquitoes.inv)
  filePath <- "Figures/vectors/qqplots/"
  for (l in 1:3){
    tryCatch({
    tiff(file = paste0(filePath, trapName[k], "_", mosqVar[k], "_", mozzyNames[l], "_norm.tiff"), width = 676, height = 563)
    qqp(mozzies[[l]], "norm")
    dev.off()
    tiff(file = paste0(filePath, trapName[k], "_", mosqVar[k], "_", mozzyNames[l], "_lnorm.tiff"), width = 676, height = 563)
    qqp(mozzies[[l]], "lnorm")
    dev.off()
    tiff(file = paste0(filePath, trapName[k], "_", mosqVar[k], "_", mozzyNames[l], "_nbinom.tiff"), width = 676, height = 563)
    nbinom <- fitdistr(mozzies[[l]], "Negative Binomial")
    qqp(mozzies[[l]], "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
    dev.off()
    tiff(file = paste0(filePath, trapName[k], "_", mosqVar[k], "_", mozzyNames[l], "_poisson.tiff"), width = 676, height = 563)
    poisson <- fitdistr(mozzies[[l]], "Poisson")
    qqp(mozzies[[l]], "pois", poisson$estimate)
    tiff(file = paste0(filePath, trapName[k], "_", mosqVar[k], "_", mozzyNames[l], "_gamma.tiff"), width = 676, height = 563)
    dev.off()
    gamma <- fitdistr(mozzies[[l]], "gamma")
    qqp(mozzies[[l]], "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])
    dev.off()
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

graphics.off() # closes all open graphing devices from loop

# variance component analysis ---------------------------------------------
library("VCA")

ae.traps2 <- list(bg.sub.ae.pos, hlc.s.sub.ae.pos, prokopack.sub.ae.pos, larvae.sub.ae, larvae.sub.ae, larvae.sub.ae, ovitrap.sub.ae.pos)
mosqVar2 <- c(rep("mosquitoes",3), "early_instars_larva", "late_instars_larva", "pupae_larva", "egg_count_ovitrap")
trapName2 <- c("bg.sub.ae.pos", "hlc.s.sub.ae.pos", "prokopack.sub.ae.pos", rep("larvae.sub.ae", 3), "ovitrap.sub.ae.pos")

vca.sp.Results <- data.frame("Mosquito_trap"=as.numeric(NA), "Response_variable"=as.numeric(NA), "site_variance"=as.numeric(NA), "village_variance"=as.numeric(NA), "house_variance"=as.numeric(NA))
vca.tm.Results <- data.frame("Mosquito_trap"=as.numeric(NA), "Response_variable"=as.numeric(NA), "year_variance"=as.numeric(NA), "month_variance"=as.numeric(NA))

for (m in 1:length(trapName2)){
  vcaDF <- as.data.frame(ae.traps2[m])
  vcaDF$mosquitoes.log <- log(vcaDF[,mosqVar2[m]]+1)
  sp.vca <- anovaVCA(mosquitoes.log ~ study_site/village_estate/vector_house_id, vcaDF)
  tm.vca <- anovaVCA(mosquitoes.log ~ year/month, vcaDF)
  sp.results <- c(trapName2[m], mosqVar2[m], sp.vca$aov.tab[22:25])
  vca.sp.Results <- rbind(vca.sp.Results, sp.results)
  tm.results <- c(trapName2[m], mosqVar2[m], tm.vca$aov.tab[18:19])
  vca.tm.Results <- rbind(vca.tm.Results, tm.results)
}

vca.sp.Results <- vca.sp.Results[c(2:nrow(vca.sp.Results)),]
vca.tm.Results <- vca.tm.Results[c(2:nrow(vca.tm.Results)),]
write.csv(vca.sp.Results, "Concatenated_Data/vector/model_output/spatialVCA_log_mosquitoes.csv", row.names = F)
write.csv(vca.tm.Results, "Concatenated_Data/vector/model_output/temporalVCA_log_mosquitoes.csv", row.names = F)

# model selection -----------------------------------------------------
source("C:/Users/Jamie/Box Sync/R_functions/glm_drop1_fn.R")
nested.str <- c("study_site", "village_estate", "vector_house_id", "year", "month")

# bg analysis ---------------------------------------------------------
bg.sub.ae.pos$logMosquitoes <- log(bg.sub.ae.pos$mosquitoes+1)
bg.response.var <- "logMosquitoes"
bg.predictor.vars <- c("no_of_rooms", "sex", "survey", "day_no", "team_lead", "fed", "insecticide_sprayed", "mosquito_coil_burn", "bed_net_present", "eaves_open", "rooms_with_ceilings", "house_wall")
bg.df <- bg.sub.ae.pos[,c(bg.response.var, bg.predictor.vars, nested.str)]
bg.df <- bg.df[complete.cases(bg.df), ]
lapply(bg.df[bg.predictor.vars], unique) # sex, team lead, insecticide sprayed, mosquito coils burned, and bed net present all contain only one level of factor
glm.select(bg.response.var, bg.predictor.vars, nested.str, bg.df)

# hlc by sex analysis -------------
hlc.s.sub.ae.pos$logMosquitoes <- log(hlc.s.sub.ae.pos$mosquitoes+1) 
hlc.response.var <- "logMosquitoes"
hlc.predictor.vars <- c("day_hlc", "collection_place_hlc", "team_lead", "collector", "sex_status", "no_of_rooms", "insecticide_sprayed", "eaves_open", "rooms_with_ceilings", "house_wall")
hlc.df <- hlc.s.sub.ae.pos[,c(hlc.response.var, hlc.predictor.vars, nested.str)]
hlc.df <- hlc.df[complete.cases(hlc.df), ]
glm.select(hlc.response.var, hlc.predictor.vars, nested.str, hlc.df)

# prokopack analysis -------------
prokopack.sub.ae.pos$logMosquitoes <- log(prokopack.sub.ae.pos$mosquitoes+1) 
prok.response.var <- "logMosquitoes"
prok.predictor.vars <- c("bushes_around_the_house", "tall_grass_around_the_house", "team_lead", "location", "sex", "fed", "no_of_rooms", "insecticide_sprayed", "mosquito_coil_burn","bed_net_present", "eaves_open", "rooms_with_ceilings", "house_wall")
prok.df <- prokopack.sub.ae.pos[,c(prok.response.var, prok.predictor.vars, nested.str)]
prok.df <- prok.df[complete.cases(prok.df), ]
glm.select(prok.response.var, prok.predictor.vars, nested.str, prok.df)

# larvae analysis -------------
lar.predictor.vars <- c("location", "roomplace_larva", "habitat_type_larva", "habitat_size_larva", "team_lead", "no_of_rooms", "insecticide_sprayed", "mosquito_coil_burn", "bed_net_present")
larvae.sub.ae$logEarlyInstars <- log(larvae.sub.ae$early_instars_larva+1) 
earlyIn.response.var <- "logEarlyInstars"
earlyIn.df <- larvae.sub.ae[,c(earlyIn.response.var, lar.predictor.vars, nested.str)]
earlyIn.df <- earlyIn.df[complete.cases(earlyIn.df), ]
glm.select(earlyIn.response.var, lar.predictor.vars, nested.str, earlyIn.df)

larvae.sub.ae$logLateInstars <- log(larvae.sub.ae$late_instars_larva+1) 
lateIn.response.var <- "logLateInstars"
lateIn.df <- larvae.sub.ae[,c(lateIn.response.var, lar.predictor.vars, nested.str)]
lateIn.df <- lateIn.df[complete.cases(lateIn.df), ]
glm.select(lateIn.response.var, lar.predictor.vars, nested.str, lateIn.df)

larvae.sub.ae$logPupae <- log(larvae.sub.ae$pupae_larva+1) 
pupae.response.var <- "logPupae"
pupae.df <- larvae.sub.ae[,c(pupae.response.var, lar.predictor.vars, nested.str)]
pupae.df <- pupae.df[complete.cases(pupae.df), ]
glm.select(pupae.response.var, lar.predictor.vars, nested.str, pupae.df)

# ovitrap analysis -------------
# can't use location because it only has two factors?
ovi.predictor.vars <- c("team_lead", "place", "no_of_rooms", "insecticide_sprayed" , "mosquito_coil_burn", "bed_net_present", "eaves_open", "rooms_with_ceilings", "house_wall")
ovi.response.var <- "egg_count_ovitrap"
ovi.df <- ovitrap.sub.ae.pos[,c(ovi.response.var, ovi.predictor.vars, nested.str)]
ovi.df <- ovi.df[complete.cases(ovi.df), ]
glm.select(ovi.response.var, ovi.predictor.vars, nested.str, ovi.df)

ovitrap.sub.ae.pos
summary(oviModelFull)

oviModelSub1 <- glm(egg_count_ovitrap ~ team_lead + place + insecticide_sprayed + mosquito_coil_burn + house_wall + month + year + (1 |study_site/village_estate/vector_house_id), family=gaussian, data=ovitrap.sub.ae.pos)
summary(oviModelSub1)

oviModelSub2 <- glm(egg_count_ovitrap ~ place + insecticide_sprayed + mosquito_coil_burn + house_wall + month + year + (1 |study_site/village_estate/vector_house_id), family=gaussian, data=ovitrap.sub.ae.pos)
summary(oviModelSub2) #place baseline = banana; insecticide baseline = no; mosquito coil baseline = no; house wall baseline = 1 (mud) 2=cement; 
