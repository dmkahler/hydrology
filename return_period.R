# Return period based on gage data
library(dplyr)
library(readr)
library(lubridate)
library(e1071)

# Unicode-8 and MacOS appear to work for text-encoding from tab-delimited file from USGS.
g <- read_tsv("monongahela_elizabethcity.u08.txt", skip = 35, col_names = FALSE)
g <- g %>%
      rename(datetime = X3, timezone = X4, gage_ft = X5, discharge_cfs = X7) %>%
      mutate(dt = as.numeric(as_datetime(datetime))) %>%
      select(-X1, -X2, -X6, -X8)

g$y <- 1
g$m <- 1
g$hydro.y <- 1
for (i in 1:nrow(g)) { # this loop can be VERY slow
      if (g$timezone[i] == "EDT") { # detect daylight savings time
            g$dt[i] <- g$dt[i] - 3600 # convert to standard time, EST
      }
      g$y[i] <- year(as_datetime(g$dt[i]))
      g$m[i] <- month(as_datetime(g$dt[i]))
      if (g$m[i] < 10) { # determine if before or after 01 October - NORTHERN HEMISPHERE
            g$hydro.y[i] <- g$y[i] # before October, current year is hydrologic year
      } else {
            g$hydro.y[i] <- g$y[i] + 1 # October on is the next hydrologic year
      }
}

z <- g %>%
      group_by(hydro.y) %>%
      summarize(q = max(discharge_cfs, na.rm = TRUE))
# write_csv(z, "mon_annual_peaks.csv")
# hist(z$q)
z <- read_csv("mon_annual_peaks.csv")

x <- z$q # random variable, peak flow
y <- log(x) # log of data
m <- mean(y)
s <- sd(y)
c <- skewness(y)
yn <- (y-m)/s # normalized log variables
cdf <- pnorm(yn,0,1)
plot(x,cdf)
