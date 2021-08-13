# For data analysis

library(ggplot2)
library(dplyr)
library(lubridate)

# Daily Precipitation, Average Temperature
daily <- data %>%
      group_by(loc.dy) %>%
      summarize(precip_mm = sum(prcp), avetemp_C = mean(temp))

# Daily Minimum and Maximum Temperature, based on meteorologic day
minmax <- data %>%
      group_by(met.dy) %>%
      summarize(mintemp_C = min(temp), maxtemp_C = max(temp))

# Monthly Data
data$ym <- 100 * data$year + data$mont