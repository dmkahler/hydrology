# This code takes data from Mutale River and sorts into seasonal bins.  As an example, the river stage is used.

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

mutale <- read_csv("Limpopo_Resilience_Lab__Mutale_Weir_Dataset.csv")

mutale <- mutale %>%
      mutate(Date_Time = ymd_hm(paste(YEAR, MONT, DAYN, HOUR, MINU))) %>%
      na_if(-9999) %>%
      na_if(-8888) %>%
      na_if(-7777) %>%
      select(-YEAR, -DAYN, -HOUR, -MINU) %>% ## NOTE: I KEEP THE MONTH VALUE, THIS IS A CHANGE FROM THE EXAMPLE FROM CLASS
      rename(Precipitation_mm = PRCP,
             AirTemp_degC = TEMP,
             RelHumidity_percent = RHMD,
             SolarRad_W_m2 = SRAD,
             AirPressure_kPa = APRS,
             WindSpeed_m_s = WSPD,
             WindDir_deg = WDIR,
             RiverStage_m = RIVS,
             WaterTemp_degC = WTMP,
             Conductivity_uS_cm = COND,
             Turbidity_NTU = TRBD)

for (i in 1:nrow(mutale)) {
      if ((mutale$MONT[i] >= 4) & (mutale$MONT[i] <= 5)) { # April to May
            mutale$season[i] <- "Autumn"
      }
      if ((mutale$MONT[i] >= 6) & (mutale$MONT[i] <= 8)) { # June to August
            mutale$season[i] <- "Winter"
      }
      if (mutale$MONT[i] == 8) { # September
            mutale$season[i] <- "Spring"
      }
      if ((mutale$MONT[i] >= 9) | (mutale$MONT[i] <= 3)) { # October to March, note the use of the OR operator, |
            mutale$season[i] <- "Summer"
      }
}

mutale$season <- factor(mutale$season, levels = c("Autumn", "Winter", "Spring", "Summer")) # This will provide the order for your plots, e.g., the x-axis order in the boxplot below

ggplot(mutale) +
      geom_boxplot(aes(x = season, y = RiverStage_m)) +
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(aspect.ratio = 1) +
      theme(axis.text = element_text(face = "plain", size = 12))
