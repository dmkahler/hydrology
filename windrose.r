## Wind data from Pittsburgh International Airport.
## Example of rose plot of wind direction

library(tidyverse) # includes ggplot and readr commands
library(RColorBrewer) # used for wind rose with color code by speed
library(lubridate) # used for dates, specifically month separation

## Pittsburgh International Airport (PIT)
pit <- read_csv("https://duq.box.com/shared/static/i9qlh63qdzf5hqvf40dphkbwkh93o2h0.csv")
pit$month <- pit$Month
pit$dir <- pit$`Wind Dir` # degrees
pit$spd <- 0.51444444444 * pit$`Wind Speed` # converted to m/s from knots, per http://www.climate.psu.edu/data/current/help.php

# OLD METHOD - no speed binning
br <- 10*(c(0:36)) # This array constructs the bins in degrees
h <- hist(pit$dir, breaks = br)
# Make rose plot, based on:
# https://stackoverflow.com/questions/39024758/how-to-use-r-package-circular-to-make-rose-plot-of-histogram-data-on-360/39025913
# https://stackoverflow.com/questions/50163352/plot-wind-rose-in-r
angle <- h$mids
count <- h$counts
y <- data.frame(angle, count)
ggplot(y, aes(x = angle, y = count)) +
      labs(caption = "Pittsburgh International Airport") +
      geom_col(fill = "steelblue", color = "steelblue") +
      coord_polar(theta = "x", start = 0) +
      scale_x_continuous(breaks = seq(0, 360, 45)) +
      theme_linedraw() +
      theme(axis.title = element_blank(), panel.ontop = TRUE, panel.background = element_blank())

## NEW METHOD - with speed binning
speed.bins <- 6
dir.bins <- 36

wind <- array(0, dim = c(speed.bins, dir.bins))
for (i in 1:nrow(pit)) {
  j <- ceiling(pit$dir[i]/10)
  k <- ceiling(pit$spd[i]/2)
  if (k > 6) { # brute force correction for speeds over 12m/s
    k <- 6
  }
  wind[k,j] <- wind[k,j] + 1
}

## Now, form long array rather than wide:
wind.long <- array(NA, dim = dir.bins*speed.bins)
speeds <- c(rep("0-2",dir.bins), rep("2-4",dir.bins), rep("4-6",dir.bins), rep("6-8",36), rep("8-10",36), rep("above 10",36)) # be sure to fill in as many as the wind bins in "wind" allocation
directions <- rep(5+10*(c(0:35)), speed.bins)
for (i in 1:speed.bins) {
  for (j in 1:dir.bins) {
    wind.long[(dir.bins*(i-1))+j] <- wind[i,j]
  }
}
rose <- data.frame(directions, speeds, wind.long)
ggplot(rose, aes(fill = fct_rev(speeds), x = directions, y = wind.long)) +
      labs(caption = paste("Pittsburgh International Airport")) +
      geom_bar(position="stack", stat="identity") +
      scale_fill_brewer("Speed (m/s)", palette = "Blues") +
      coord_polar(theta = "x", start = 0) +
      scale_x_continuous(breaks = seq(0, 360, 45)) +
      theme_linedraw() +
      theme(axis.title = element_blank(), panel.ontop = TRUE, panel.background = element_blank()) # NOTE: ylim used in export


