# merge case data ---------------------------------------
rm(list=ls()) #remove previous variable assignments

# load case data
cases_kenya <- read.csv("Concatenated_Data/case_data/aic_cases_by_month_Kenya.csv", head=T, stringsAsFactors = F)
cases_ecuador <- read.csv("Concatenated_Data/case_data/cases_by_month_Ecuador.csv", head=T, stringsAsFactors = F)

# format columns to combine data
cases_kenya$denv_positive_clinically_diagnosed <- NA

# merge data
cases <- rbind(cases_ecuador, cases_kenya, by=c("Site", "Date", "Year.Month"), all=T)

# save data
write.csv(cases, "Concatenated_Data/case_data/merged_case_data.csv", row.names = F)
