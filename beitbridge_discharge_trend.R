library(readr)
library(ggplot2)
library(dplyr)

x <- read_csv("/Users/davidkahler/Documents/Hydrology_and_WRM/Limpopo_Basin_Study/Land_Movement_Gabi/beitbridge_discharge.csv")
y <- x %>%
     select(Year,`Height (m)`) %>%
     rename(height=`Height (m)`)

m <- lm(y$height~y$Year)
summarize(m)

ggplot(y) +
     geom_point(aes(x=Year,y=height)) +
     geom_smooth(aes(x=Year,y=height), method = "lm", se = TRUE, color='blue') +
     xlab("Hydrologic Year") +
     ylab("Mean Gauge Height (m)") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12)) +
     theme(axis.title = element_text(face = "plain", size = 12))


