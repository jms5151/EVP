#------ Plot vector data
rm(list=ls()) #remove previous variable assignments

library(ggplot2)
library(plyr)

#--- read in vector data
bg <- read.csv("Concatenated_Data/BG_Regrssion_Data.csv", head=T, as.is=T)
hlc <- read.csv("Concatenated_Data/HLC_Regrssion_Data.csv", head=T, as.is=T)
larvae <- read.csv("Concatenated_Data/Larvae_Regrssion_Data.csv", head=T, as.is=T)
ovitrap <- read.csv("Concatenated_Data/Ovitrap_Regrssion_Data.csv", head=T, as.is=T)
prokopak <- read.csv("Concatenated_Data/Prokopak_Regrssion_Data.csv", head=T, as.is=T)

#--- plot data
#-- bg
bg$pickupDate <- as.Date(bg$pickupDate, "%Y-%m-%d")
bg$mon.yr <- format(bg$pickupDate, "%Y-%m")
bg$month <- format(bg$pickupDate, "%m")

monthlyBG <- ddply(bg, .(mon.yr, study_site),
                    summarise,
                    mean.Unfed = mean(na.omit(a.aegypti_unfed)),
                    se.Unfed = sd(na.omit(a.aegypti_unfed))/sqrt(sum(na.omit(a.aegypti_unfed))),
                    mean.Female = mean(na.omit(a.aegypti_female)),
                    se.Female = sd(na.omit(a.aegypti_female))/sqrt(sum(na.omit(a.aegypti_female))),
                    mean.Total = mean(na.omit(a.aegypti_total)),
                    se.Total = sd(na.omit(a.aegypti_total))/sqrt(sum(na.omit(a.aegypti_total))))

pdf("Figures/vectors/BG_aegypti_unfed_by_monthYr.pdf", width=8.5, height=11)
#jpeg("Figures/vectors/BG_aegypti_unfed_by_monthYr.jpeg", width=767, height=587)
ggplot(monthlyBG, aes(mon.yr, mean.Unfed, color=study_site)) + geom_bar(stat="identity") + 
  facet_wrap(~ study_site, ncol=1) +
  theme(axis.text.x=element_text(angle=60,vjust=0.5)) +
  labs(x="Date", y="Unfed a. aegypti (count)") +
  geom_errorbar(aes(ymin=mean.Unfed-se.Unfed, ymax=mean.Unfed+se.Unfed),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
dev.off()

pdf("Figures/vectors/BG_aegypti_female_by_monthYr.pdf", width=8.5, height=11)
ggplot(monthlyBG, aes(mon.yr, mean.Female, color=study_site)) + geom_bar(stat="identity") + 
  facet_wrap(~ study_site, ncol=1) +
  theme(axis.text.x=element_text(angle=60,vjust=0.5)) +
  labs(x="Date", y="Female a. aegypti (count)") +
  geom_errorbar(aes(ymin=mean.Female-se.Female, ymax=mean.Female+se.Female),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
dev.off()

pdf("Figures/vectors/BG_aegypti_total_by_monthYr.pdf", width=8.5, height=11)
ggplot(monthlyBG, aes(mon.yr, mean.Total, color=study_site)) + geom_bar(stat="identity") + 
  facet_wrap(~ study_site, ncol=1) +
  theme(axis.text.x=element_text(angle=60,vjust=0.5)) +
  labs(x="Date", y="Total a. aegypti (count)") +
  geom_errorbar(aes(ymin=mean.Total-se.Total, ymax=mean.Total+se.Total),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
dev.off()

aveMonBG <- ddply(bg, .(month, study_site),
                   summarise,
                   mean.Unfed = mean(na.omit(a.aegypti_unfed)),
                   se.Unfed = sd(na.omit(a.aegypti_unfed))/sqrt(sum(na.omit(a.aegypti_unfed))),
                   mean.Female = mean(na.omit(a.aegypti_female)),
                   se.Female = sd(na.omit(a.aegypti_female))/sqrt(sum(na.omit(a.aegypti_female))),
                   mean.Total = mean(na.omit(a.aegypti_total)),
                   se.Total = sd(na.omit(a.aegypti_total))/sqrt(sum(na.omit(a.aegypti_total))))

pdf("Figures/vectors/BG_aegypti_unfed_by_month.pdf", width=8.5, height=11)
ggplot(aveMonBG, aes(month, mean.Unfed, color=study_site)) + geom_bar(stat="identity") + 
  facet_wrap(~ study_site, ncol=1) +
  theme(axis.text.x=element_text(angle=60,vjust=0.5)) +
  labs(x="Month", y="Unfed a. aegypti (count)") +
  geom_errorbar(aes(ymin=mean.Unfed-se.Unfed, ymax=mean.Unfed+se.Unfed),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
dev.off()

pdf("Figures/vectors/BG_aegypti_female_by_month.pdf", width=8.5, height=11)
ggplot(aveMonBG, aes(month, mean.Female, color=study_site)) + geom_bar(stat="identity") + 
  facet_wrap(~ study_site, ncol=1) +
  theme(axis.text.x=element_text(angle=60,vjust=0.5)) +
  labs(x="Month", y="Female a. aegypti (count)") +
  geom_errorbar(aes(ymin=mean.Female-se.Female, ymax=mean.Female+se.Female),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
dev.off()

pdf("Figures/vectors/BG_aegypti_total_by_month.pdf", width=8.5, height=11)
ggplot(aveMonBG, aes(month, mean.Total, color=study_site)) + geom_bar(stat="identity") + 
  facet_wrap(~ study_site, ncol=1) +
  theme(axis.text.x=element_text(angle=60,vjust=0.5)) +
  labs(x="Month", y="Total a. aegypti (count)") +
  geom_errorbar(aes(ymin=mean.Total-se.Total, ymax=mean.Total+se.Total),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
dev.off()

#-- hlc 
hlc$end_date <- as.Date(hlc$end_date, "%Y-%m-%d")
hlc$mon.yr <- format(hlc$end_date, "%Y-%m")
hlc$month <- format(hlc$end_date, "%m")

monthlyHLC <- ddply(hlc, .(mon.yr, study_site),
                   summarise,
                   mean.Unfed = mean(na.omit(a.aegypti_unfed)),
                   se.Unfed = sd(na.omit(a.aegypti_unfed))/sqrt(sum(na.omit(a.aegypti_unfed))),
                   mean.bloodfed = mean(na.omit(a.aegypti_bloodfed)),
                   se.bloodfed = sd(na.omit(a.aegypti_bloodfed))/sqrt(sum(na.omit(a.aegypti_bloodfed))),
                   mean.Female = mean(na.omit(a.aegypti_female)),
                   se.Female = sd(na.omit(a.aegypti_female))/sqrt(sum(na.omit(a.aegypti_female))),
                   mean.Total = mean(na.omit(a.aegypti_total)),
                   se.Total = sd(na.omit(a.aegypti_total))/sqrt(sum(na.omit(a.aegypti_total))))

pdf("Figures/vectors/HLC_aegypti_unfed_by_monthYr.pdf", width=8.5, height=11)
ggplot(monthlyHLC, aes(mon.yr, mean.Unfed, color=study_site)) + geom_bar(stat="identity") + 
  facet_wrap(~ study_site, ncol=1) +
  theme(axis.text.x=element_text(angle=60,vjust=0.5)) +
  labs(x="Date", y="Unfed a. aegypti (count)") +
  geom_errorbar(aes(ymin=mean.Unfed-se.Unfed, ymax=mean.Unfed+se.Unfed),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
dev.off()

pdf("Figures/vectors/HLC_aegypti_female_by_monthYr.pdf", width=8.5, height=11)
ggplot(monthlyHLC, aes(mon.yr, mean.Female, color=study_site)) + geom_bar(stat="identity") + 
  facet_wrap(~ study_site, ncol=1) +
  theme(axis.text.x=element_text(angle=60,vjust=0.5)) +
  labs(x="Date", y="Female a. aegypti (count)") +
  geom_errorbar(aes(ymin=mean.Female-se.Female, ymax=mean.Female+se.Female),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
dev.off()

pdf("Figures/vectors/HLC_aegypti_total_by_monthYr.pdf", width=8.5, height=11)
ggplot(monthlyHLC, aes(mon.yr, mean.Total, color=study_site)) + geom_bar(stat="identity") + 
  facet_wrap(~ study_site, ncol=1) +
  theme(axis.text.x=element_text(angle=60,vjust=0.5)) +
  labs(x="Date", y="Total a. aegypti (count)") +
  geom_errorbar(aes(ymin=mean.Total-se.Total, ymax=mean.Total+se.Total),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
dev.off()

aveMonHLC <- ddply(hlc, .(month, study_site),
                    summarise,
                    mean.Unfed = mean(na.omit(a.aegypti_unfed)),
                    se.Unfed = sd(na.omit(a.aegypti_unfed))/sqrt(sum(na.omit(a.aegypti_unfed))),
                    mean.bloodfed = mean(na.omit(a.aegypti_bloodfed)),
                    se.bloodfed = sd(na.omit(a.aegypti_bloodfed))/sqrt(sum(na.omit(a.aegypti_bloodfed))),
                    mean.Female = mean(na.omit(a.aegypti_female)),
                    se.Female = sd(na.omit(a.aegypti_female))/sqrt(sum(na.omit(a.aegypti_female))),
                    mean.Total = mean(na.omit(a.aegypti_total)),
                    se.Total = sd(na.omit(a.aegypti_total))/sqrt(sum(na.omit(a.aegypti_total))))

pdf("Figures/vectors/HLC_aegypti_unfed_by_month.pdf", width=8.5, height=11)
ggplot(aveMonHLC, aes(month, mean.Unfed, color=study_site)) + geom_bar(stat="identity") + 
  facet_wrap(~ study_site, ncol=1) +
  theme(axis.text.x=element_text(angle=60,vjust=0.5)) +
  labs(x="Month", y="Unfed a. aegypti (count)") +
  geom_errorbar(aes(ymin=mean.Unfed-se.Unfed, ymax=mean.Unfed+se.Unfed),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
dev.off()

pdf("Figures/vectors/HLC_aegypti_female_by_month.pdf", width=8.5, height=11)
ggplot(aveMonHLC, aes(month, mean.Female, color=study_site)) + geom_bar(stat="identity") + 
  facet_wrap(~ study_site, ncol=1) +
  theme(axis.text.x=element_text(angle=60,vjust=0.5)) +
  labs(x="Month", y="Female a. aegypti (count)") +
  geom_errorbar(aes(ymin=mean.Female-se.Female, ymax=mean.Female+se.Female),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
dev.off()

pdf("Figures/vectors/HLC_aegypti_total_by_month.pdf", width=8.5, height=11)
ggplot(aveMonHLC, aes(month, mean.Total, color=study_site)) + geom_bar(stat="identity") + 
  facet_wrap(~ study_site, ncol=1) +
  theme(axis.text.x=element_text(angle=60,vjust=0.5)) +
  labs(x="Month", y="Total a. aegypti (count)") +
  geom_errorbar(aes(ymin=mean.Total-se.Total, ymax=mean.Total+se.Total),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
dev.off()

#-- larvae
larvae$Date <- as.Date(larvae$Date, "%Y-%m-%d")
larvae$mon.yr <- format(larvae$Date, "%Y-%m")
larvae$month <- format(larvae$Date, "%m")

monthlyLar <- ddply(larvae, .(mon.yr, study_site),
                    summarise,
                    mean.Early.Instars = mean(na.omit(early_instars)),
                    se.Early.Instars = sd(na.omit(early_instars))/sqrt(sum(na.omit(early_instars))),
                    mean.Late.Instars = mean(na.omit(late_instars)),
                    se.Late.Instars = sd(na.omit(late_instars))/sqrt(sum(na.omit(late_instars))),
                    mean.Pupae = mean(na.omit(pupae)),
                    se.Pupae = sd(na.omit(pupae))/sqrt(sum(na.omit(pupae))))

pdf("Figures/vectors/Larvae_early_instars_by_monthYr.pdf", width=8.5, height=11)
ggplot(monthlyLar, aes(mon.yr, mean.Early.Instars, color=study_site)) + geom_bar(stat="identity") + 
  facet_wrap(~ study_site, ncol=1) +
  theme(axis.text.x=element_text(angle=60,vjust=0.5)) +
  labs(x="Date", y="Early instars (count)") +
  geom_errorbar(aes(ymin=mean.Early.Instars-se.Early.Instars, ymax=mean.Early.Instars+se.Early.Instars),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
dev.off()

pdf("Figures/vectors/Larvae_late_instars_by_monthYr.pdf", width=8.5, height=11)
ggplot(monthlyLar, aes(mon.yr, mean.Late.Instars, color=study_site)) + geom_bar(stat="identity") + 
  facet_wrap(~ study_site, ncol=1) +
  theme(axis.text.x=element_text(angle=60,vjust=0.5)) +
  labs(x="Date", y="Late instars (count)") +
  geom_errorbar(aes(ymin=mean.Late.Instars-se.Late.Instars, ymax=mean.Late.Instars+se.Late.Instars),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
dev.off()

pdf("Figures/vectors/Larvae_pupae_by_monthYr.pdf", width=8.5, height=11)
ggplot(monthlyLar, aes(mon.yr, mean.Pupae, color=study_site)) + geom_bar(stat="identity") + 
  facet_wrap(~ study_site, ncol=1) +
  theme(axis.text.x=element_text(angle=60,vjust=0.5)) +
  labs(x="Date", y="Pupae (count)") +
  geom_errorbar(aes(ymin=mean.Pupae-se.Pupae, ymax=mean.Pupae+se.Pupae),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
dev.off()

aveMonLar <- ddply(larvae, .(month, study_site),
                    summarise,
                    mean.Early.Instars = mean(na.omit(early_instars)),
                    se.Early.Instars = sd(na.omit(early_instars))/sqrt(sum(na.omit(early_instars))),
                    mean.Late.Instars = mean(na.omit(late_instars)),
                    se.Late.Instars = sd(na.omit(late_instars))/sqrt(sum(na.omit(late_instars))),
                    mean.Pupae = mean(na.omit(pupae)),
                    se.Pupae = sd(na.omit(pupae))/sqrt(sum(na.omit(pupae))))

pdf("Figures/vectors/Larvae_early_instars_by_month.pdf", width=8.5, height=11)
ggplot(aveMonLar, aes(month, mean.Early.Instars, color=study_site)) + geom_bar(stat="identity") + 
  facet_wrap(~ study_site, ncol=1) +
  theme(axis.text.x=element_text(angle=60,vjust=0.5)) +
  labs(x="Date", y="Early instars (count)") +
  geom_errorbar(aes(ymin=mean.Early.Instars-se.Early.Instars, ymax=mean.Early.Instars+se.Early.Instars),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
dev.off()

pdf("Figures/vectors/Larvae_late_instars_by_month.pdf", width=8.5, height=11)
ggplot(aveMonLar, aes(month, mean.Late.Instars, color=study_site)) + geom_bar(stat="identity") + 
  facet_wrap(~ study_site, ncol=1) +
  theme(axis.text.x=element_text(angle=60,vjust=0.5)) +
  labs(x="Date", y="Late instars (count)") +
  geom_errorbar(aes(ymin=mean.Late.Instars-se.Late.Instars, ymax=mean.Late.Instars+se.Late.Instars),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
dev.off()

pdf("Figures/vectors/Larvae_pupae_by_month.pdf", width=8.5, height=11)
ggplot(aveMonLar, aes(month, mean.Pupae, color=study_site)) + geom_bar(stat="identity") + 
  facet_wrap(~ study_site, ncol=1) +
  theme(axis.text.x=element_text(angle=60,vjust=0.5)) +
  labs(x="Date", y="Pupae (count)") +
  geom_errorbar(aes(ymin=mean.Pupae-se.Pupae, ymax=mean.Pupae+se.Pupae),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
dev.off()

#-- ovitrap
ovitrap$Date <- as.Date(ovitrap$date_collected)
ovitrap$mon.yr <- format(ovitrap$Date, "%Y-%m")
ovitrap$eggRate <- ovitrap$eggs/ovitrap$num.days
ovitrap$month <- format(ovitrap$Date, "%m")

monthlyOvi <- ddply(ovitrap, .(mon.yr, study_site),
              summarise,
              totalEggs = sum(na.omit(eggs)),
              totalEggsSE = sd(na.omit(eggs))/sqrt(sum(na.omit(eggs))),
              dailyEggRate = sum(na.omit(eggRate)),
              dailyEggSE = sd(na.omit(eggRate))/sqrt(sum(na.omit(eggRate))))

pdf("Figures/vectors/Ovitrap_total_eggs_by_monthYr.pdf", width=8.5, height=11)
ggplot(monthlyOvi, aes(mon.yr, totalEggs, color=study_site)) + geom_bar(stat="identity") + 
  facet_wrap(~ study_site, ncol=1) +
  theme(axis.text.x=element_text(angle=60,vjust=0.5)) +
  labs(x="Date", y="Eggs (count)") +
  geom_errorbar(aes(ymin=totalEggs-totalEggsSE, ymax=totalEggs+totalEggsSE),
              width=.2,                    # Width of the error bars
              position=position_dodge(.9))
dev.off()

pdf("Figures/vectors/Ovitrap_total_egg_rate_by_monthYr.pdf", width=8.5, height=11)
ggplot(monthlyOvi, aes(mon.yr, dailyEggRate, color=study_site)) + geom_bar(stat="identity") + 
  facet_wrap(~ study_site, ncol=1) +
  theme(axis.text.x=element_text(angle=60,vjust=0.5)) +
  labs(x="Date", y="Eggs rate") +
  geom_errorbar(aes(ymin=dailyEggRate-dailyEggSE, ymax=dailyEggRate+dailyEggSE),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
dev.off()

aveMonOvi <- ddply(ovitrap, .(month, study_site),
             summarise,
             meanEggs = mean(na.omit(eggs)),
             meanEggRate = mean(na.omit(eggRate)),
             seEggs = sd(na.omit(eggs))/sqrt(length(na.omit(eggs))),
             seEggRate = sd(na.omit(eggRate))/sqrt(length(na.omit(eggRate))))

pdf("Figures/vectors/Ovitrap_total_eggs_by_month.pdf", width=8.5, height=11)
ggplot(aveMonOvi, aes(month, meanEggs, color=study_site)) + geom_bar(stat="identity") + 
  facet_wrap(~ study_site, ncol=1) +
  theme(axis.text.x=element_text(angle=60,vjust=0.5)) +
  labs(x="Month", y="Eggs (mean)") +
  geom_errorbar(aes(ymin=meanEggs-seEggs, ymax=meanEggs+seEggs),
              width=.2,                    # Width of the error bars
              position=position_dodge(.9))
dev.off()

pdf("Figures/vectors/Ovitrap_egg_rate_by_month.pdf", width=8.5, height=11)
ggplot(aveMonOvi, aes(month, meanEggRate, color=study_site)) + geom_bar(stat="identity") + 
  facet_wrap(~ study_site, ncol=1) +
  theme(axis.text.x=element_text(angle=60,vjust=0.5)) +
  labs(x="Month", y="Egg rate") +
  geom_errorbar(aes(ymin=meanEggRate-seEggRate, ymax=meanEggRate+seEggRate),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
dev.off()


#-- prokopak
prokopak$Date <- as.Date(prokopak$Date, "%Y-%m-%d")
prokopak$mon.yr <- format(prokopak$Date, "%Y-%m")
prokopak$month <- format(prokopak$Date, "%m")

monthlyPro <- ddply(prokopak, .(mon.yr, study_site),
                   summarise,
                   mean.Unfed = mean(na.omit(a.aegypti_unfed)),
                   se.Unfed = sd(na.omit(a.aegypti_unfed))/sqrt(sum(na.omit(a.aegypti_unfed))),
                   mean.Female = mean(na.omit(a.aegypti_female)),
                   se.Female = sd(na.omit(a.aegypti_female))/sqrt(sum(na.omit(a.aegypti_female))),
                   mean.Total = mean(na.omit(a.aegypti_total)),
                   se.Total = sd(na.omit(a.aegypti_total))/sqrt(sum(na.omit(a.aegypti_total))))

pdf("Figures/vectors/Prokopak_aegypti_unfed_by_monthYr.pdf", width=8.5, height=11)
ggplot(monthlyPro, aes(mon.yr, mean.Unfed, color=study_site)) + geom_bar(stat="identity") + 
  facet_wrap(~ study_site, ncol=1) +
  theme(axis.text.x=element_text(angle=60,vjust=0.5)) +
  labs(x="Date", y="Unfed a. aegypti (count)") +
  geom_errorbar(aes(ymin=mean.Unfed, ymax=mean.Unfed+se.Unfed),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
dev.off()

pdf("Figures/vectors/Prokopak_aegypti_female_by_monthYr.pdf", width=8.5, height=11)
ggplot(monthlyPro, aes(mon.yr, mean.Female, color=study_site)) + geom_bar(stat="identity") + 
  facet_wrap(~ study_site, ncol=1) +
  theme(axis.text.x=element_text(angle=60,vjust=0.5)) +
  labs(x="Date", y="Female a. aegypti (count)") +
  geom_errorbar(aes(ymin=mean.Female, ymax=mean.Female+se.Female),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
dev.off()

pdf("Figures/vectors/Prokopak_aegypti_total_by_monthYr.pdf", width=8.5, height=11)
ggplot(monthlyPro, aes(mon.yr, mean.Total, color=study_site)) + geom_bar(stat="identity") + 
  facet_wrap(~ study_site, ncol=1) +
  theme(axis.text.x=element_text(angle=60,vjust=0.5)) +
  labs(x="Date", y="Total a. aegypti (count)") +
  geom_errorbar(aes(ymin=mean.Total, ymax=mean.Total+se.Total),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
dev.off()

aveMonPro <- ddply(prokopak, .(month, study_site),
                  summarise,
                  mean.Unfed = mean(na.omit(a.aegypti_unfed)),
                  se.Unfed = sd(na.omit(a.aegypti_unfed))/sqrt(sum(na.omit(a.aegypti_unfed))),
                  mean.Female = mean(na.omit(a.aegypti_female)),
                  se.Female = sd(na.omit(a.aegypti_female))/sqrt(sum(na.omit(a.aegypti_female))),
                  mean.Total = mean(na.omit(a.aegypti_total)),
                  se.Total = sd(na.omit(a.aegypti_total))/sqrt(sum(na.omit(a.aegypti_total))))

pdf("Figures/vectors/Prokopak_aegypti_unfed_by_month.pdf", width=8.5, height=11)
ggplot(aveMonPro, aes(month, mean.Unfed, color=study_site)) + geom_bar(stat="identity") + 
  facet_wrap(~ study_site, ncol=1) +
  theme(axis.text.x=element_text(angle=60,vjust=0.5)) +
  labs(x="Month", y="Unfed a. aegypti (count)") +
  geom_errorbar(aes(ymin=mean.Unfed, ymax=mean.Unfed+se.Unfed),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
dev.off()

pdf("Figures/vectors/Prokopak_aegypti_female_by_month.pdf", width=8.5, height=11)
ggplot(aveMonPro, aes(month, mean.Female, color=study_site)) + geom_bar(stat="identity") + 
  facet_wrap(~ study_site, ncol=1) +
  theme(axis.text.x=element_text(angle=60,vjust=0.5)) +
  labs(x="Month", y="Female a. aegypti (count)") +
  geom_errorbar(aes(ymin=mean.Female, ymax=mean.Female+se.Female),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
dev.off()

pdf("Figures/vectors/Prokopak_aegypti_total_by_month.pdf", width=8.5, height=11)
ggplot(aveMonPro, aes(month, mean.Total, color=study_site)) + geom_bar(stat="identity") + 
  facet_wrap(~ study_site, ncol=1) +
  theme(axis.text.x=element_text(angle=60,vjust=0.5)) +
  labs(x="Month", y="Total a. aegypti (count)") +
  geom_errorbar(aes(ymin=mean.Total, ymax=mean.Total+se.Total),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
dev.off()


#---- plot chulaimbo for agu
library(plotly)
chuPro <- subset(monthlyPro, study_site="Chulaimbo")
chuOvi <- subset(monthlyOvi, study_site="Chulaimbo")
chuLar <- subset(monthlyLar, study_site="Chulaimbo")
chuLar$instars <- na.omit(chuLar$mean.Early.Instars)+na.omit(chuLar$mean.Late.Instars)

eggs <- plot_ly() %>%
  add_trace(data=chuOvi, x = ~mon.yr, y = ~totalEggs, type = 'bar', color=I('maroon'))%>%
  layout(yaxis = list(side = 'left', title = 'Eggs', showgrid = FALSE, showline=TRUE), xaxis=list(showline=TRUE, showgrid = FALSE))

instars <- plot_ly() %>%
  add_trace(data=chuLar, x = ~mon.yr, y = ~instars, type = 'bar', color=I('darkslategrey'))%>%
  layout(yaxis = list(side = 'left', title = 'Instars', showgrid = FALSE, showline=TRUE), xaxis=list(showline=TRUE, showgrid = FALSE))

pupae <- plot_ly() %>%
  add_trace(data=chuLar, x = ~mon.yr, y = ~mean.Pupae, type = 'bar', color=I('deepskyblue4'))%>%
  layout(yaxis = list(side = 'left', title = 'Pupae', showgrid = FALSE, showline=TRUE), xaxis=list(showline=TRUE, showgrid = FALSE))

adults <- plot_ly() %>%
  add_trace(data=chuPro, x = ~mon.yr, y = ~mean.Total, type = 'bar', color=I('darkorchid4'))%>%
  layout(yaxis = list(side = 'left', title = 'Adults', showgrid = FALSE, showline=TRUE), xaxis=list(showline=TRUE, showgrid = FALSE))

subplot(eggs, instars, pupae, adults, nrows = 4, shareX = T, titleX = FALSE, titleY = TRUE)%>%layout(showlegend= FALSE)

#--- plot ovitrap rainfall data
chulaimbo <- read.csv("Concatenated_Data/climate/GapFilled_Daily_T_Chulaimbo.csv", head=T)
chulaimbo$mon.yr <- format(as.Date(chulaimbo$Date, "%Y-%m-%d"), "%Y-%m")
kisumu <- read.csv("Concatenated_Data/climate/GapFilled_Daily_T_Kisumu.csv", head=T)
kisumu$mon.yr <- format(as.Date(kisumu$Date, "%Y-%m-%d"), "%Y-%m")
msambweni <- read.csv("Concatenated_Data/climate/GapFilled_Daily_T_Msambweni.csv", head=T)
msambweni$mon.yr <- format(as.Date(msambweni$Date, "%Y-%m-%d"), "%Y-%m")
ukunda <- read.csv("Concatenated_Data/climate/GapFilled_Daily_T_Ukunda.csv", head=T)
ukunda$mon.yr <- format(as.Date(ukunda$Date, "%Y-%m-%d"), "%Y-%m")

rainEggs <- monthlyOvi[,c("mon.yr", "totalEggs", "study_site")]
rainEggs$rain <- "NA"

for (i in 1:nrow(rainEggs)){
  if (rainEggs$study_site[i] == "Chulaimbo"){
    rainset <- chulaimbo
  } else if (rainEggs$study_site[i] == "Kisumu"){
    rainset <- kisumu
  } else if (rainEggs$study_site[i] == "Msambweni"){
    rainset <- msambweni
  } else if (rainEggs$study_site[i] == "Ukunda"){
    rainset <- ukunda
  }
}  
merge(monthlyOvi[,c("mon.yr", "totalEggs")], chulaimbo[,c("mon.yr", "rainfall_hobo")], by=)

