

library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
# install_github("LimpopoLab/hydrostats")
library(hydrostats)

x <- read_csv("A7H008.csv", col_names = FALSE)

ggplot(x) +
  geom_point(aes(x=X1,y=X4))
