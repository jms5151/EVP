# Download rainfall data from ncep ARC2 --------------------------------------------------------
library(RCurl)

# get list of ftp links to download data
links <- getURL("ftp://ftp.cpc.ncep.noaa.gov/fews/fewsdata/africa/arc2/geotiff/",verbose=TRUE,dirlistonly = TRUE) 
links <- unlist(strsplit(links, "[\\\\]|[^[:print:]]",fixed=FALSE))
links <- links[grep(".zip", links)]

# create folder for file downloads if it does not already exist
subDir <- "Kenya_historical/Kenya_ARC2_zips"

if (!file.exists(subDir)){
  dir.create(file.path(subDir))
}

# download data
for (i in 1:length(links)){ 
  ARC2.file <- links[i] 
  ARC2.URL <- paste0("ftp://ftp.cpc.ncep.noaa.gov/fews/fewsdata/africa/arc2/geotiff/", ARC2.file)
  ARC2.directory <- paste0("Kenya_historical/Kenya_ARC2_zips/", ARC2.file)
  download.file(ARC2.URL, mode="wb", ARC2.directory)
}