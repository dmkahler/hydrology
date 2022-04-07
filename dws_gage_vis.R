

library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
# install_github("LimpopoLab/hydrostats")
library(hydrostats)

x <- read_csv("A7H004.csv", col_names = FALSE)
x <- x %>%
     rename(dt=X1,UNIX=X2,lev_m=X3,levQC=X4,flow_m3s=X5,flowQC=X6) # note UNIX date time is UTC

y <- read_csv("A7H008.csv", col_names = FALSE)
y <- y %>%
     rename(dt=X1,UNIX=X2,lev_m=X3,levQC=X4,flow_m3s=X5,flowQC=X6) # note UNIX date time is UTC

z <- rbind(x,y)
q <- z %>%
     mutate(lev_m=replace(lev_m, which(lev_m<=0),NA)) %>%
     mutate(flow_m3s=replace(flow_m3s, which(flow_m3s<=0),NA)) %>%
     group_by(UNIX) %>%
     summarize(lev_m=mean(lev_m,na.rm=TRUE), levQC=mean(levQC), flow_m3s=mean(flow_m3s,na.rm=TRUE), flowQC=mean(flowQC)) %>%
     mutate(dt=with_tz(as_datetime(UNIX), tzone = "Africa/Johannesburg"))

ggplot(q) +
  geom_point(aes(x=dt,y=flow_m3s))

## Annual flood
a <- q %>%
     mutate(hydro_year=hyd.yr(dt, h = "S"))
