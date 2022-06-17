library(readr)
library(devtools)
library(hydrostats)
library(lubridate)
library(dplyr)
library(ggplot2)
library(doParallel)

setwd("/Volumes/T7/pa_hydro/")
# https://www.ncei.noaa.gov/weather-climate-links
# Integrated Surface Database 
pall <- read_csv("3000361.csv", col_types = "ccTcccccc")
# https://www.ncei.noaa.gov/data/global-hourly/doc/isd-format-document.pdf 

allegheny <- pall %>%
     filter(NAME=="PITTSBURGH ALLEGHENY CO AIRPORT, PA US")
pgh <- pall %>%
     filter(NAME=="PITTSBURGH ASOS, PA US")

allegheny$precip <- NA
allegheny$temp <- NA
registerDoParallel(detectCores())
allegheny.precip <- foreach (i = 1:nrow(allegheny), .combine = rbind) %dopar% {
     if (is.na(allegheny$AA1[i])==FALSE){
          parsing <- strsplit(allegheny$AA1[i],",")
          parsing <- parsing[[1]][2]
          parsing <- as.numeric(parsing)
          if (parsing > 9998) {
               parsing <- NA
          }
          dt <- as.character(as_datetime(allegheny$DATE[i]))
          print(c(dt,parsing)) # will output to parallel output, unfortunately, everything will be in char
     }
}
allegheny.precip <- data.frame(allegheny.precip)
ac.daily.precip <- allegheny.precip %>%
     rename(dt=X1,prcp=X2) %>%
     mutate(pr=as.numeric(prcp)) %>%
     mutate(d=as_date(dt)) #%>%
     group_by(d) %>%
     summarize(p=sum(pr, na.rm = TRUE))

pgh$precip <- NA
pgh$temp <- NA
registerDoParallel(detectCores())
pgh.precip <- foreach (i = 1:nrow(pgh), .combine = rbind) %dopar% {
     if (is.na(pgh$AA1[i])==FALSE){
          parsing <- strsplit(pgh$AA1[i],",")
          parsing <- parsing[[1]][2]
          parsing <- as.numeric(parsing)
          if (parsing>9998) {
               parsing <- NA
          }
          dt <- as.character(as_datetime(pgh$DATE[i]))
          print(c(dt,parsing)) # will output to parallel output
     }
}
pgh.precip <- data.frame(pgh.precip)

