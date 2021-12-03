# analyze river flow in Kruger
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

mamba <- read_csv("Olifants_Mamba_B7H015.csv", skip = 9, n_max = 12174) # upstream
mamba <- mamba %>% 
      mutate(dn = as.numeric(date)) %>% 
      select(-date) %>% 
      mutate(year = floor(dn/10000)) %>% 
      mutate(mon = floor((dn-year*10000)/100)) %>% 
      mutate(day = dn-(10000*year+100*mon)) %>% 
      mutate(dt = ymd(dn)) %>% 
      select(-dn)

balule <- read_csv("Olifants_Balule_B7H026.csv", skip = 9) # downstream, near Olifants Camp
balule <- balule %>% 
      mutate(dn = as.numeric(date)) %>% 
      select(-date) %>% 
      mutate(year = floor(dn/10000)) %>% 
      mutate(mon = floor((dn-year*10000)/100)) %>% 
      mutate(day = dn-(10000*year+100*mon)) %>% 
      mutate(dt = ymd(dn)) %>% 
      select(-dn)

# Start comparison table
start <- as.numeric(min(c(min(mamba$dt), min(balule$dt))))
stop <- as.numeric(max(c(max(mamba$dt), max(mamba$dt))))
dur <- as.numeric(stop) - as.numeric(start) + 1
dn <- c(start:stop)
mamba.discharge <- array(NA, dim = dur)
balule.discharge <- array(NA, dim = dur)
for (i in 1:nrow(mamba)) {
      dateposition <- as.numeric(mamba$dt[i]) - start + 1
      mamba.discharge[dateposition] <- mamba$discharge[i]
}
for (i in 1:nrow(balule)) {
      dateposition <- as.numeric(balule$dt[i]) - start + 1
      balule.discharge[dateposition] <- balule$discharge[i]
}
comp <- data.frame(dn, mamba.discharge, balule.discharge)

# plot
ggplot(comp, aes(x = as_date(dn))) + 
      geom_point(aes(y = mamba.discharge)) +
      geom_point(aes(y = balule.discharge))
