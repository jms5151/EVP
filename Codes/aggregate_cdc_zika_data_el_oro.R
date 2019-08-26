# concatenate Zika data -----------------------------------------
x <- list.files("Concatenated_Data/cdc_zika_data/")

cdcZika <- data.frame()

for (i in 1:length(x)){
  fileName <- paste0("Concatenated_Data/cdc_zika_data/", x[i])
  x2 <- read.csv(fileName, head=T, stringsAsFactors = F)
  x2 <- x2[grep("^Ecuador-El_Oro", x2$location),1:9]
  colnames(x2)[1] <- "report_date" # sometimes this column does not read in correctly because of extra spaces
  cdcZika <- rbind(cdcZika, x2)
}

cdcZika$Date <- as.Date(cdcZika$report_date, "%Y-%m-%d")
cdcZika$YrMonth <- format(cdcZika$Date, "%Y-%m")
  
cdcZika2 <- cdcZika %>%
  as_tibble() %>%
  # filter(location == "Ecuador-El_Oro-Machala" | location == "Ecuador-El_Oro-Huaquillas")
  filter(location == "Ecuador-El_Oro" & data_field == "total_zika_confirmed_cumulative") %>%
  group_by(YrMonth) %>%
  summarise(confirmed_cases_Zika = sum(value, na.rm=T),
            Date = max(Date))

save(cdcZika2, file="Concatenated_Data/CDC_Zika_El_Oro_Total.RData")
