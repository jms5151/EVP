# merge vector data --------------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load library
library(purrr)

# load vector data
adults_kenya <- read.csv("Concatenated_Data/vector_data/Kenya_prokopack.csv", head=T, stringsAsFactors = F)
adults_kenya_total <- read.csv("Concatenated_Data/vector_data/Kenya_prokopack2.csv", head=T, stringsAsFactors = F)
adults_kenya_bg <- read.csv("Concatenated_Data/vector_data/Kenya_bg.csv", head=T, stringsAsFactors = F)
adults_ecuador <- read.csv("Concatenated_Data/vector_data/vector_data_Ecuador.csv", head=T, stringsAsFactors = F)
larvae <- read.csv("Concatenated_Data/vector_data/Kenya_larvae.csv", head=T, stringsAsFactors = F)
eggs <- read.csv("Concatenated_Data/vector_data/Kenya_ovitrap.csv", head=T, stringsAsFactors = F)

# combine vector data
vectors <- rbind(adults_kenya[,c("Site", "Date", "Year.Month", "aedes_total")], adults_ecuador[,c("Site", "Date",  "Year.Month","aedes_total")])
vectors <- Reduce(function(x, y) merge(x, y, by=c("Site", "Date", "Year.Month"), all=T), list(vectors, adults_kenya_total, adults_kenya_bg, larvae, eggs))

# remove year-month variable
vectors$Year.Month <- NULL

# format date
vectors$Date <- as.Date(vectors$Date, "%Y-%m-%d")

# save data
save(vectors, file="Concatenated_Data/vector_data/merged_vector_data.RData", row.names = F)
