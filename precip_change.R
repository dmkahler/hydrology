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
pgh.precip.fm15 <- pgh.precip %>%
     rename(dt=X1,type=X2,hrs=X3,prcp=X4) %>%
     mutate(cnt=1) %>%
     mutate(prcp=(as.numeric(prcp))/10) %>% # data reported in mm with a scale factor of 10.
     filter(is.na(prcp)==FALSE) %>%
     filter(type=="FM-15") %>% # hourly data?
     filter(hrs==1)

pgh.precip.sao <- pgh.precip %>%
     rename(dt=X1,type=X2,hrs=X3,prcp=X4) %>%
     mutate(cnt=1) %>%
     mutate(prcp=(as.numeric(prcp))/10) %>% # data reported in mm with a scale factor of 10.
     filter(is.na(prcp)==FALSE) %>%
     filter(type=="SAO") %>% # hourly data?
     filter(hrs==1)

pgh.precip.1hr <- rbind(pgh.precip.sao,pgh.precip.fm15)

pgh.ann.precip <- pgh.precip.1hr %>%
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
     ylim(c(0,1500)) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12)) +
     theme(axis.title = element_text(face = "plain", size = 12))
# Average precip in Pittsburgh is 1.2 m
write_csv(pgh.ann.precip, "pgh.ann.precip.csv")

pgh.daily.precip <- pgh.precip.1hr %>%
     mutate(d=as_date(dt)) %>%
     group_by(d) %>%
     summarize(p=sum(prcp, na.rm = TRUE),
               c=sum(cnt, na.rm = TRUE)) %>%
     filter(c>=20) # QA to make sure enough data were collected.

ggplot(pgh.daily.precip) +
     geom_point(aes(x=d,y=p)) +
     xlab("Date") +
     ylab("Daily Precipitation (mm)") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12)) +
     theme(axis.title = element_text(face = "plain", size = 12))
write_csv(pgh.daily.precip, "pgh.daily.precip.csv")

p2y <- evi(pgh.daily.precip$p,(1-(1/730))) # Find the 2-year precipitation event, 31.47 mm
pgh.p2y <- pgh.daily.precip %>%
     filter(p>p2y) %>%
     select(-c) %>%
     mutate(c=1) %>% # counting days
     mutate(y=hyd.yr(d, h = "N")) %>%
     group_by(y) %>%
     summarize(c=sum(c, na.rm = TRUE))

p1y <- evi(pgh.precip.1hr$prcp,(1-(1/(2*24*365)))) # 1-hour precip for 1-year event
pgh.p1yh <- pgh.precip.1hr %>%
     filter(prcp>p1y) %>%
     select(dt,prcp) %>%
     mutate(c=1) %>% # counting days
     mutate(y=hyd.yr(dt, h = "N")) %>%
     group_by(y) %>%
     summarize(c=sum(c, na.rm = TRUE))
plot(pgh.p1yh$y,pgh.p1yh$c)
ggplot(pgh.p1yh, aes(x=y,y=c)) +
     geom_point(aes(x=y,y=c)) +
     xlab("Date") +
     ylab("Number of exceedences per year") +
     geom_smooth(method = "loess") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12)) +
     theme(axis.title = element_text(face = "plain", size = 12))
geom_smooth(method = "lm", se = TRUE, color='orange')
geom_smooth(method = "loess")


