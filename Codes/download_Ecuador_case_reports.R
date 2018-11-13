# Download case reports from Ecuador MoH -----------------------------------------
rm(list=ls()) #remove previous variable assignments

# load library
library(rvest) 

# get page links for 2013-2016 case reports
pages.2013.2016 = read_html("https://www.salud.gob.ec/gaceta-epidemiologica-ecuador-sive-alerta/")
links.2013.2016 <- pages.2013.2016 %>% html_nodes("a") %>% html_attr("href")

# get page links for 2017-2018 case reports
pages.2017.2018 = read_html("https://www.salud.gob.ec/gacetas-vectoriales-2014-2015-2016-2017/")
links.2017.2018 <- pages.2017.2018 %>% html_nodes("a") %>% html_attr("href")

# combine vectors of hyperlinks
links <- c(links.2013.2016, links.2017.2018)

# remove any hyperlinks that are not PDFs
links <- links[grepl(".pdf$", links)]

# download case reports; links 162, 170, 172, 182, 221, & 272 go to a 404 page
for (i in 1:length(links)){ 
  MoH.URL <- links[i] 
  MoH.report.name <- paste0("Ecuador/Ecuador_case_reports/Case_report_", i, ".pdf")
  download.file(MoH.URL, mode="wb", MoH.report.name)
}


