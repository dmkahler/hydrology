library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)

# This program will download and parse disaster data from EM-DAT

# Limpopo Resilience Lab
# This work was supported by the United States Agency for International Development, Southern Africa Regional 
# Mission, Fixed Amount Award 72067419FA00001. This work reflects the work of the authors and does not 
# necessarily reflect the views of USAID or the United States Government.

# Static data location - data available via Box link, service courtesy of Duquesne University.
disasters <- read_csv("https://duq.box.com/shared/static/48g71l89th4prmlnmp5wupkksaf29gzy.csv", skip = 6, col_names = TRUE, col_types = "ciifffffcfffffccfffccnnfcccciiiiiiiiiiinnnn")
# Additional information on read_csv {readr} is found at: https://readr.tidyverse.org/reference/read_delim.html
# NB: local time, column 27, are not inputed as times, must import as character
# NB: longitude and latitude have inconsistant directions (N/S, E/W not uniformly used), must import as a character

# How many disasters have occurred?
h <- hist(disasters$Year, breaks = (1900.5:2021.5), main = , xlab = "Year", ylab = "Counts")
# Additional information on hist {base} is found at: https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/hist
ggplot(disasters, aes(x = Year)) + 
      geom_histogram(binwidth = 1) +
      labs(x = "Year", y = "Number of Global Disasters") +
      theme(panel.background = element_blank(), panel.border = element_rect(fill = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.background = element_rect(fill = NA), legend.key = element_rect(fill = NA))
# Alternatively, use ggplot for imaging: https://ggplot2.tidyverse.org/reference/geom_histogram.html

drought <- filter(disasters, `Disaster Type` == "Drought") %>% arrange(Year)


