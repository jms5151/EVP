# calculate r, percentiles, and z-scores to assess correspondance between models and data
rm(list=ls()) #remove previous variable assignments

# load libraries ----------------------------------------------------------
library(plyr)

# functions ---------------------------------------------------------------
source("C:/Users/Jamie/Box Sync/R_functions/standard_error.R")
calc.percentile <- function(x){ecdf(x)(x)}
z.score <- function(num, mean, sd){((num-mean)/sd)}

# load data ---------------------------------------------------------------
trait.df <- read.csv("Kenya/Concatenated_Data/SEI-SEIR/SEI-SEIR_simulations_with_trait_variation.csv", head=T, stringsAsFactors = F)
aic_cases <- read.csv("Kenya/Concatenated_Data/aic_cases_by_month.csv", head=T, stringsAsFactors = F)
adults.site <- read.csv("Kenya/Concatenated_Data/vector/site_vector_totals_adults.csv", head=T, stringsAsFactors = F)
larvae.site <- read.csv("Kenya/Concatenated_Data/vector/site_vector_totals_larvae.csv", head=T, stringsAsFactors = F)
eggs.site <- read.csv("Kenya/Concatenated_Data/vector/site_vector_totals_eggs.csv", head=T, stringsAsFactors = F)

early.instar.site <- larvae.site[,c("Date", "Year.Month", "Site", "early_instar_total")]
late.instar.site <- larvae.site[,c("Date", "Year.Month", "Site", "late_instar_total")]
pupae.site <- larvae.site[,c("Date", "Year.Month", "Site", "pupae_total")]

# sei-seir model simulation data ------------------------------------------
trait.df$Mtot <- rowSums(trait.df[,c("M1", "M2", "M3")])
trait.df <- subset(trait.df, !is.na(Site))

traitvar <- ddply(trait.df, .(Date, Site)
                  , summarize
                  , mean.I = mean(I)
                  , min.I = mean.I  - se(I)
                  , max.I = mean.I + se(I)
                  , mean.M = mean(Mtot)
                  , min.M = mean.M - se(Mtot)
                  , max.M = mean.M + se(Mtot))

traitvar <- traitvar[complete.cases(traitvar),]

# calculate little percentile, little r, and zscores for sim data --------
sites <- unique(traitvar$Site)
mosqsums <- c("mean.I", "min.I", "max.I", "mean.M", "min.M", "max.M")
newcols <- c("mean.I_r", "mean.M_r", "mean.M_zscore", "min.M_zscore", "max.M_zscore", "mean.I_zscore", "min.I_zscore", "max.I_zscore")

rDF <- data.frame()

for (i in 1:length(sites)){
  simsub <- subset(traitvar, Site == sites[i])
  # calculate percentiles
  for (j in 1:length(mosqsums)){
    newcolname <- paste0(mosqsums[j], "_percentile")
    simsub$x <- calc.percentile(simsub[,mosqsums[j]])
    colnames(simsub)[ncol(simsub)] <- newcolname
  }
  # add new columns for little r and zscore
  for(k in 1:length(newcols)){
    simsub[,newcols[k]] <- NA
  }
  for (l in 45:nrow(simsub)){
    # calculate little r (growth rate/slope)
    simsub$mean.I_r[l] <- (simsub$mean.I[(l+44)] - simsub$mean.I[(l-44)])/90
    simsub$mean.M_r[l] <- (simsub$mean.M[(l+44)] - simsub$mean.M[(l-44)])/90
    # calculate z-scores
    simsub$mean.M_zscore[l] <- z.score(simsub$mean.M[l], mean(simsub$mean.M), sd(simsub$mean.M))
    simsub$min.M_zscore[l] <- z.score(simsub$min.M[l], mean(simsub$min.M), sd(simsub$min.M))
    simsub$max.M_zscore[l] <- z.score(simsub$max.M[l], mean(simsub$max.M), sd(simsub$max.M))
    simsub$mean.I_zscore[l] <- z.score(simsub$mean.I[l], mean(simsub$mean.I), sd(simsub$mean.I))
    simsub$min.I_zscore[l] <- z.score(simsub$min.I[l], mean(simsub$min.I), sd(simsub$min.I))
    simsub$max.I_zscore[l] <- z.score(simsub$max.I[l], mean(simsub$max.I), sd(simsub$max.I))
  }
  rDF <- rbind(rDF, simsub)
}

r.perc.z <- grep("Date|Site|percentile|zscore|r", names(rDF), value = TRUE)
traitvar2 <- merge(traitvar, rDF[,r.perc.z], by=c("Date", "Site"))

# calculate percentile, little r, and zscores for case & vector data -------
fieldDFs <- list(aic_cases, aic_cases, adults.site, pupae.site, late.instar.site, early.instar.site, eggs.site)
yvars <- c("denv_positive", "chikv_positive", "aedes_total", "pupae_total", "late_instar_total", "early_instar_total", "egg_total")
newDFnames <- c("DENV", "CHIKV", "Adults", "Pupae", "LateInstars", "EarlyInstars", "Eggs")

for (p in 1:length(fieldDFs)){
  field.df <- data.frame()
  fielddat <- fieldDFs[[p]]
  fielddat$r <- NA
  fielddat$z <- NA
  for (q in 1:length(sites)){
    fieldsub <- subset(fielddat, Site == sites[q])
    # calculate percentiles
    fieldsub$x <- ecdf(fieldsub[,yvars[p]])(fieldsub[,yvars[p]])
    colnames(fieldsub)[ncol(fieldsub)] <- paste0(yvars[p], "_percentile")
    for (r in 1:nrow(fieldsub)){
      # calculate zscores
      fieldsub$z[r] <- z.score(fieldsub[r, yvars[p]], mean(fieldsub[,yvars[p]]), sd(fieldsub[,yvars[p]]))
      if (r > 1 & r < (nrow(fieldsub)-1)){
        # calculate little r
        fieldsub$r[r] <- (fieldsub[(r+1), yvars[p]] - fieldsub[(r-1), yvars[p]])/3
      }
    }
    colnames(fieldsub)[which(colnames(fieldsub)=="z")] <- paste0(yvars[p], "_zscore")
    colnames(fieldsub)[which(colnames(fieldsub)=="r")] <- paste0(yvars[p], "_r")
    field.df <- rbind(field.df, fieldsub)
  }
  assign(newDFnames[p], field.df)
}

# connecting datasets for comparisons -------------------------------------------
fieldDFs2 <- list(DENV, CHIKV, Adults, Pupae, LateInstars, EarlyInstars, Eggs)

for (s in 1:length(fieldDFs2)){
  field.df2 <- fieldDFs2[[s]]
  if (newDFnames[s] == "DENV" | newDFnames[s] == "CHIKV"){
    field.df2$Date <- paste0(field.df2$Year.Month, "-28")
  }
  tempDF <- merge(traitvar2, field.df2, by=c("Site", "Date"))
  fileName <- paste0("Kenya/Concatenated_Data/SEI-SEIR/Correspondence_", newDFnames[s], ".csv")
  write.csv(tempDF, fileName, row.names = F)
  assign(paste0(newDFnames[s], "2"), tempDF)
}

# plotting comparisons ---------------------------------------------------------
compDFs <- list(DENV2, CHIKV2, Adults2, Pupae2, LateInstars2, EarlyInstars2, Eggs2)
titles <- c("r", "percentile", "zscore")

for (t in 1:length(compDFs)){
  tempDF <- compDFs[[t]]
  name_r <- paste0(yvars[t], "_r")
  name_p <- paste0(yvars[t], "_percentile")
  name_z <- paste0(yvars[t], "_zscore")
  datanames <- c(name_r, name_p, name_z)
  if (yvars[t] == "denv_positive" | yvars[t] == "chikv_positive"){
    seiR <- "mean.I_r"
    seiP <- "mean.I_percentile"
    seiZ <- "mean.I_zscore"
  } else {
    seiR <- "mean.M_r"
    seiP <- "mean.M_percentile"
    seiZ <- "mean.M_zscore"
  }
  seinames <- c(seiR, seiP, seiZ)
  for (u in 1:length(titles)){
    minlim <- min(tempDF[,datanames[u]], tempDF[,seinames[u]], na.rm=T)
    maxlim <- max(tempDF[,datanames[u]], tempDF[,seinames[u]], na.rm=T)
    filename <- paste0("Kenya/Figures/SEI-SEIR-Output/model_v_data/Correspondance_", yvars[t], "_", titles[u], ".tiff")
    tiff(filename, width = 714, height = 504)
    plot(tempDF[,datanames[u]], tempDF[,seinames[u]], pch=21, bg="lightblue", xlab=datanames[u], ylab=seinames[u], main=yvars[t], ylim=c(minlim,maxlim), xlim=c(minlim,maxlim))
    abline(0,1)
    dev.off()
  }
}

# combine and save data for plotting data overlaying simulations ------------------------------------
simobs <- merge(traitvar2, DENV2[,c("Date", "Site", "denv_positive")], by=c("Date", "Site"), all.x=T)
simobs <- merge(simobs, CHIKV2[,c("Date", "Site", "chikv_positive")], by=c("Date", "Site"), all.x=T)
simobs <- merge(simobs, Adults2[,c("Date", "Site", "aedes_total")], by=c("Date", "Site"), all.x=T)
simobs <- merge(simobs, Pupae2[,c("Date", "Site", "pupae_total")], by=c("Date", "Site"), all.x=T)
simobs <- merge(simobs, LateInstars2[,c("Date", "Site", "late_instar_total")], by=c("Date", "Site"), all.x=T)
simobs <- merge(simobs, EarlyInstars2[,c("Date", "Site", "early_instar_total")], by=c("Date", "Site"), all.x=T)
simobs <- merge(simobs, Eggs2[,c("Date", "Site", "egg_total")], by=c("Date", "Site"), all.x=T)
write.csv(simobs, "Kenya/Concatenated_Data/model_v_data.csv", row.names = F)
