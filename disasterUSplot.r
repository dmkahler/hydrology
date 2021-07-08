library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gt)

disasters <- read_csv("https://duq.box.com/shared/static/48g71l89th4prmlnmp5wupkksaf29gzy.csv", skip = 6, col_names = TRUE, col_types = "ciifffffcfffffccfffccnnfcccciiiiiiiiiiinnnn")

damages <- disasters %>%
  filter(Country == "United States of America (the)") %>% # dplyr::filter keeps only rows from USA
  group_by(Year) %>% # group_by will pass the data sorted by year and will cause summarize to sum by year
  summarize(tdam = 1e-6 * sum(`Total Damages ('000 US$)`, na.rm = TRUE)) # NOTE: I have changed this to billions USD, it is no longer thousands of dollars.
#  Here, tdam is a new variable within damages (data.frame) that is constructed by the command given.
#  summarize is unique because it both adds a new column/variable and collapses the overall number of rows

ggplot(damages, aes(x = Year, y = tdam)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Year", y = "Total Damages (billion USD)") +
  xlim(1900,2025) +
  theme(panel.background = element_blank(), panel.border = element_rect(fill = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank())


