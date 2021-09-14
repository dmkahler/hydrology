library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(latex2exp)
library(ncdf4) # see more: https://www.r-bloggers.com/2016/08/a-netcdf-4-in-r-cheatsheet/

# Local data
nc1 <- nc_open("sgp30ebbrE13.b1.20210801.000000.nc")
nc2 <- nc_open("sgp30ebbrE13.b1.20210802.000000.nc")

# Box storage
nc1 <- nc_open("https://duq.box.com/shared/static/gwhjd43heviy3icstqb158uc8zai1oll.nc")
nc2 <- nc_open("https://duq.box.com/shared/static/900oimvwcc3m6hcov1xs3agf1oz24o2y.nc")

# Data from:
# Atmospheric Radiation Measurement (ARM) user facility. 2016. Energy 
# Balance Bowen Ratio Station (30EBBR). 1993-07-04 to 2021-09-06, 
# Southern Great Plains (SGP) Lamont, OK (Extended and Co-located 
# with C1) (E13). Compiled by D. Cook, R. Sullivan, D. Whiteman, E. 
# Keeler and B. Ermold. ARM Data Center. Data set accessed 2021-09-08 
# at http://dx.doi.org/10.5439/1025337.

#attributes(nc1)$names
#attributes(nc1$var)$names

bt1 <- ncvar_get(nc1, attributes(nc1$var)$names[1]) # base time
bt2 <- ncvar_get(nc2, attributes(nc2$var)$names[1]) # base time
ts1 <- ncvar_get(nc1, attributes(nc1$var)$names[2]) # time (seconds)
ts2 <- ncvar_get(nc2, attributes(nc2$var)$names[2]) # time (seconds)
s1 <- length(ts1)
s2 <- length(ts2)
dt <- array(NA, dim = (s1+s2))
for (i in 1:s1) {
      dt[i] <- ((bt1+ts1[i]-(6*3600)) - as.numeric(ymd_hms("2021-08-01T00:00:00"))) / 3600 # hours relative to local midnight 01 Aug 2021
}
for (i in (1:s2)) {
      dt[s1+i] <- ((bt2+ts2[i]-(6*3600)) - as.numeric(ymd_hms("2021-08-01T00:00:00"))) / 3600 # hours relative to local midnight 01 Aug 2021
}
rm(bt1, bt2, ts1, ts2, s1, s2)

netr <- c(ncvar_get(nc1, attributes(nc1$var)$names[22]), ncvar_get(nc2, attributes(nc2$var)$names[22])) # net ratiation
bowr <- c(ncvar_get(nc1, attributes(nc1$var)$names[98]), ncvar_get(nc2, attributes(nc2$var)$names[98])) # Bowen ratio
leef <- c(ncvar_get(nc1, attributes(nc1$var)$names[100]), ncvar_get(nc2, attributes(nc2$var)$names[100])) # latent heat flux
shef <- c(ncvar_get(nc1, attributes(nc1$var)$names[102]), ncvar_get(nc2, attributes(nc2$var)$names[102])) # sensible heat flux

dat <- data.frame(dt, netr, leef, shef, bowr)
ggplot(dat, aes(x = dt)) +
      geom_line(aes(y = netr, color = "Net Radiation")) +
      geom_line(aes(y = leef, color = "Evaporation")) +
      geom_line(aes(y = shef, color = "Sensible Heat")) +
      labs(color = "Flux", x = "Hour", y = TeX('Power ($W/m^2$)'), title = "01 Aug 2021, Oklahoma, USA") +
      xlim(0,24) +
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(aspect.ratio = 1) +
      theme(axis.text = element_text(face = "plain", size = 12))

ggplot(dat, aes(x = dt)) +
      geom_line(aes(y = bowr)) +
      labs(x = "Hour", y = "Bowen Ratio", title = "01 Aug 2021, Oklahoma, USA") +
      xlim(0,24) +
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(aspect.ratio = 1) +
      theme(axis.text = element_text(face = "plain", size = 12))

