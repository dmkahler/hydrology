library(readr)
library(devtools)
library(hydrostats)
library(lubridate)
library(dplyr)
library(ggplot2)
library(doParallel)

# Comparison of NCEI data for Pittsburgh

# https://www.ncei.noaa.gov/weather-climate-links
# Integrated Surface Database 
pall <- read_csv("/Users/davidkahler/Documents/Hydrology_and_WRM/river_data/precip/3000361.csv", col_types = "ccTcccccc")
# https://www.ncei.noaa.gov/data/global-hourly/doc/isd-format-document.pdf 

allegheny <- pall %>%
     filter(NAME=="PITTSBURGH ALLEGHENY CO AIRPORT, PA US")
pgh <- pall %>%
     filter(NAME=="PITTSBURGH ASOS, PA US")

pgh$precip <- NA
pgh$temp <- NA
registerDoParallel(detectCores())
pgh.precip <- foreach (i = 1:nrow(pgh), .combine = rbind) %dopar% {
     if (is.na(pgh$AA1[i])==FALSE){
          dt <- as.character(as_datetime(pgh$DATE[i]))
          parsing <- strsplit(pgh$AA1[i],",")
          hrs <- as.numeric(parsing[[1]][1])
          prcp <- as.numeric(parsing[[1]][2])
          if (prcp>9998) {
               prcp <- NA
          }
          type <- pgh$REPORT_TYPE[i]
          print(c(dt,type,hrs,prcp)) # will output to parallel output
     }
}
pgh.precip <- data.frame(pgh.precip)
pgh.precip.test <- pgh.precip %>%
     rename(dt=X1,type=X2,hrs=X3,prcp=X4) %>%
     mutate(cnt=1) %>%
     mutate(prcp=(as.numeric(prcp))/10) %>% # data reported in mm with a scale factor of 10.
     filter(is.na(prcp)==FALSE)

pgh.ann.precip <- pgh.precip %>%
     mutate(y=hyd.yr(dt, h = "N")) %>%
     group_by(y) %>%
     summarize(p=sum(prcp, na.rm = TRUE), 
               c=sum(cnt, na.rm = TRUE)) %>%
     filter(c>=8000) # QA to make sure enough data were collected.
# the c value should approach the number of hours in a year: 365*24=8760

ggplot(pgh.ann.precip) +
     geom_col(aes(x=y,y=p)) +
     xlab("Hydrologic Year") +
     ylab("Annual Precipitation (mm)") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12)) +
     theme(axis.title = element_text(face = "plain", size = 12))
# Average precip in Pittsburgh is 1.2 m
write_csv(pgh.ann.precip, "pgh.ann.precip.csv")

pgh.daily.precip <- pgh.precip %>%
     rename(dt=X1,prcp=X2) %>%
     mutate(pr=as.numeric(prcp)) %>%
     mutate(d=as_date(dt)) %>%
     group_by(d) %>%
     summarize(p=sum(pr, na.rm = TRUE))

ggplot(pgh.daily.precip) +
     geom_point(aes(x=d,y=p)) +
     xlab("Date") +
     ylim(c(-0.5,0.5)) +
     ylab("w'") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12)) +
     theme(axis.title = element_text(face = "plain", size = 12))
write_csv(pgh.daily.precip, "pgh.daily.precip.csv")
