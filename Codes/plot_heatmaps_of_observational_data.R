# load data and combine data
rm(list=ls()) #remove previous variable assignments

# load libraries
library(dplyr)
library(ggplot2)

# load multiplot function
load("C:/Users/Jamie/Box Sync/R_functions/multiplot.R")

# load data
cases <- read.csv("Concatenated_Data/case_data/merged_case_data.csv", head=T, stringsAsFactors = F)
vectors <- read.csv("Concatenated_Data/vector_data/merged_vector_data.csv", head=T, stringsAsFactors = F)

# reduce rows for same month-year with different dates
vectors2 <- vectors %>% group_by(Site, Year.Month) %>% summarise_all(funs(.[!is.na(.)][1]))

# set scaling function
range01 <- function(x){(x-min(x, na.rm=T))/(max(x, na.rm=T)-min(x, na.rm=T))}

# scale data
cases_scaled <- cases %>% group_by(Site) %>% mutate(denv_positive_LC = range01(denv_positive)
                                                    , denv_positive_CD = range01(denv_positive_clinically_diagnosed)
                                                    , chikv_positive_LC = range01(chikv_positive))


vectors_scaled <- vectors2 %>% group_by(Site) %>% mutate(aedes_prok = range01(aedes_total)
                                                        , aedes_bg = range01(aedes_total_bg)
                                                        , pupae = range01(pupae_total)
                                                        , early = range01(early_instar_total)
                                                        , late = range01(late_instar_total)
                                                        , eggs = range01(egg_total_adjusted))


# plot
# http://www.roymfrancis.com/a-guide-to-elegant-tiled-heatmaps-in-r-2019/
col1 = "lightblue" 
col2 = "darkblue"

vectors_scaled$Site <- factor(vectors_scaled$Site, levels=c("Zaruma", "Portovelo", "Machala", "Huaquillas", "Ukunda", "Msambweni", "Kisumu", "Chulaimbo"))

ggplot(vectors_scaled, aes(x=Year.Month, y=Site)) + geom_tile(aes(fill = aedes_prok), colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2, na.value = 'white') +  
  guides(fill=guide_legend(title="")) +
  theme_bw() + theme_minimal() +
  labs(title = "Aedes aegypti (prokopack)", x = "", y = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_discrete(breaks=c("2014-01", "2015-01", "2016-01", "2017-01", "2018-01"))

vectors_scaled2 <-subset(vectors_scaled, Site == "Chulaimbo"|Site=="Kisumu"|Site=="Msambweni"|Site=="Ukunda")
vectors_scaled2$Site <- factor(vectors_scaled2$Site, levels=c("Ukunda", "Msambweni", "Kisumu", "Chulaimbo"))

eggs <- ggplot(vectors_scaled2, aes(x=Year.Month, y=Site)) + geom_tile(aes(fill = eggs), colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2, na.value = 'white') +  
  guides(fill=guide_legend(title="")) +
  theme_bw() + theme_minimal() +
  labs(title = "Aedes aegypti eggs", x = "", y = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_discrete(breaks=c("2014-01", "2015-01", "2016-01", "2017-01", "2018-01"))
  # scale_x_discrete(breaks=c("2002-01", "2003-01", "2004-01", "2005-01", "2006-01", "2007-01", "2008-01", "2009-01", "2010-01", "2011-01", "2012-01", "2013-01", "2014-01", "2015-01", "2016-01", "2017-01", "2018-01"))

early <- ggplot(vectors_scaled2, aes(x=Year.Month, y=Site)) + geom_tile(aes(fill = early), colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2, na.value = 'white') +  
  guides(fill=guide_legend(title="")) +
  theme_bw() + theme_minimal() +
  labs(title = "Aedes aegypti early instars", x = "", y = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_discrete(breaks=c("2014-01", "2015-01", "2016-01", "2017-01", "2018-01"))

late <- ggplot(vectors_scaled2, aes(x=Year.Month, y=Site)) + geom_tile(aes(fill = late), colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2, na.value = 'white') +  
  guides(fill=guide_legend(title="")) +
  theme_bw() + theme_minimal() +
  labs(title = "Aedes aegypti late instars", x = "", y = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_discrete(breaks=c("2014-01", "2015-01", "2016-01", "2017-01", "2018-01"))

pupae <- ggplot(vectors_scaled2, aes(x=Year.Month, y=Site)) + geom_tile(aes(fill = pupae), colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2, na.value = 'white') +  
  guides(fill=guide_legend(title="")) +
  theme_bw() + theme_minimal() +
  labs(title = "Aedes aegypti pupae", x = "", y = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_discrete(breaks=c("2014-01", "2015-01", "2016-01", "2017-01", "2018-01"))

bg <- ggplot(vectors_scaled2, aes(x=Year.Month, y=Site)) + geom_tile(aes(fill = aedes_bg), colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2, na.value = 'white') +  
  guides(fill=guide_legend(title="")) +
  theme_bw() + theme_minimal() +
  labs(title = "Aedes aegypti (BG)", x = "", y = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_discrete(breaks=c("2014-01", "2015-01", "2016-01", "2017-01", "2018-01"))

multiplot(bg, pupae, late, early, eggs)

cases_scaled$Year <- substr(cases_scaled$Year.Month, 1, 4)
cases_scaled2 <- subset(cases_scaled, Year >= "2014")
cases_scaled2$Site <- factor(cases_scaled2$Site, levels=c("Zaruma", "Portovelo", "Machala", "Huaquillas", "Ukunda", "Msambweni", "Kisumu", "Chulaimbo"))

LCd <- ggplot(cases_scaled2, aes(x=Year.Month, y=Site)) + geom_tile(aes(fill = denv_positive_LC), colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2, na.value = 'white') +  
  guides(fill=guide_legend(title="")) +
  theme_bw() + theme_minimal() +
  labs(title = "Dengue (lab-confirmed)", x = "", y = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_discrete(breaks=c("2014-01", "2015-01", "2016-01", "2017-01", "2018-01"))

LCc <- ggplot(cases_scaled2, aes(x=Year.Month, y=Site)) + geom_tile(aes(fill = chikv_positive_LC), colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2, na.value = 'white') +  
  guides(fill=guide_legend(title="")) +
  theme_bw() + theme_minimal() +
  labs(title = "Chikungunya (lab-confirmed)", x = "", y = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_discrete(breaks=c("2014-01", "2015-01", "2016-01", "2017-01", "2018-01"))

multiplot(LCd, LCc)

cases_scaled3 <-subset(cases_scaled, Site == "Huaquillas"|Site=="Machala"|Site=="Portovelo"|Site=="Zaruma")
cases_scaled3$Site <- factor(cases_scaled3$Site, levels=c("Zaruma", "Portovelo", "Machala", "Huaquillas"))

ggplot(cases_scaled3, aes(x=Year.Month, y=Site)) + geom_tile(aes(fill = denv_positive_CD), colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2, na.value = 'white') +  
  guides(fill=guide_legend(title="")) +
  theme_bw() + theme_minimal() +
  labs(title = "Dengue (clinically suspected)", x = "", y = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_discrete(breaks=c("2002-01", "2003-01", "2004-01", "2005-01", "2006-01", "2007-01", "2008-01", "2009-01", "2010-01", "2011-01", "2012-01", "2013-01", "2014-01", "2015-01", "2016-01", "2017-01", "2018-01"))
