

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

ggplot(z) +
  geom_point(aes(x=dt,y=flow_m3s))
