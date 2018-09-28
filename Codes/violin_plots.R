library(ggplot2)
vector.df <- read.csv("Results/bootstrapped_Regression_Model_v_Traps.csv", head=T) 
vector.df.null26 <- read.csv("Results/bootstrapped_Regression_Model_v_Traps_Null_25.9C.csv", head=T)
vector.df.null29 <- read.csv("Results/bootstrapped_Regression_Model_v_Traps_Null_29C.csv", head=T)

vector.df$mosquito_metric <- gsub("a.aegypti_total", "adults", vector.df$mosquito_metric)
vector.df.null26$mosquito_metric <- gsub("a.aegypti_total", "adults", vector.df.null26$mosquito_metric)
vector.df.null29$mosquito_metric <- gsub("a.aegypti_total", "adults", vector.df.null29$mosquito_metric)

fitmeasurements <- c("mean_fit", "RootMeanSqErr", "MeanAbsErr")#"sameMagnitude", "oneMagnitudeDiff", "twoMagnitudeDiff", "threeMagnitudeDiff")

#---- weekly violin plots
wk.df<-subset(vector.df, timespan=="weekly.model.Total")
wk.df<-subset(wk.df, trap=="hlc"|trap=="Larvae"|trap=="Ovitrap")

for (measure in fitmeasurements){
  filepath <- paste0("Figures/vectors/violin_plots/", measure, "_weekly.tiff")
  # filepath <- paste0("Figures/vectors/violin_plots/", measure, "_weekly_Null_25.9C.tiff")
  # filepath <- paste0("Figures/vectors/violin_plots/", measure, "_weekly_Null_29C.tiff")
  tiff(filepath, width=756, height=529)
  ggplot(wk.df, aes(x=mosquito_metric, y=wk.df[,measure], fill=mosquito_metric)) + 
    geom_violin(trim=FALSE) + 
    scale_x_discrete(limits=c("adults", "pupae", "instars", "Eggs")) + labs(y = measure) + coord_flip()
  dev.off()
}

#---- monthly violin plots
mn.df<-subset(vector.df, timespan=="monthly.model.Total")
mn.df<-subset(mn.df, trap=="hlc"|trap=="Larvae"|trap=="Ovitrap")

for (measure in fitmeasurements){
  filepath <- paste0("Figures/vectors/violin_plots/", measure, "_monthly.tiff")
  # filepath <- paste0("Figures/vectors/violin_plots/", measure, "_monthly_Null_25.9C.tiff")
  # filepath <- paste0("Figures/vectors/violin_plots/", measure, "_monthly_Null_29C.tiff")
  tiff(filepath, width=756, height=529)
  ggplot(mn.df, aes(x=mosquito_metric, y=mn.df[,measure], fill=mosquito_metric)) + 
    geom_violin(trim=FALSE) + 
    scale_x_discrete(limits=c("adults", "pupae", "instars", "Eggs")) + labs(y = measure) + coord_flip()
  dev.off()
}
  

#--- overlay two violin plots
wk.df.null26<-subset(vector.df.null26, timespan=="weekly.model.Total")
wk.df.null26<-subset(wk.df.null26, trap=="hlc"|trap=="Larvae"|trap=="Ovitrap")

p <- ggplot(wk.df, aes(factor(mosquito_metric), RootMeanSqErr))
p <- p + geom_violin(aes(colour = "#1268FF"), alpha = 0.3)
q <- p + geom_violin(aes(y = MeanAbsErr, colour = "#3268FF"), alpha = 0.3)
q + coord_flip()

ggplot() +
  geom_violin(data=wk.df, aes(factor(mosquito_metric), RootMeanSqErr))+
  geom_violin(data=wk.df.null26, aes(factor(mosquito_metric), RootMeanSqErr,colour=mosquito_metric))+

#---- histograms
ggplot(data=wk.df, aes(RootMeanSqErr)) + geom_histogram()

ggplot(mn.df, aes(RootMeanSqErr,  color=mosquito_metric, fill=mosquito_metric)) +
  geom_histogram(alpha = 0.3) + 
  facet_wrap(~ mosquito_metric, ncol = 2)

ggplot() + 
  geom_histogram(data=wk.df, aes(x=RootMeanSqErr, y=..density..),  fill="lightblue") + facet_wrap(~mosquito_metric, ncol = 2) +
  geom_histogram(data=wk.df.null26, aes(x=RootMeanSqErr, y=..density..),  fill="darkblue", alpha=0.1) + facet_wrap(~mosquito_metric, ncol = 2)

#---- merge test
df <- merge(vector.df[,c("RootMeanSqErr", "mosquito_metric", "timespan")], 
        vector.df.null26[,c("RootMeanSqErr", "mosquito_metric", "timespan")])
df<-data.frame(df)
