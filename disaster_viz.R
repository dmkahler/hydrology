library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gt) # used for table export.  For more: https://blog.rstudio.com/2020/04/08/great-looking-tables-gt-0-2/ 

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
      theme(panel.background = element_blank(), panel.border = element_rect(fill = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# Alternatively, use ggplot for imaging: https://ggplot2.tidyverse.org/reference/geom_histogram.html

# Table data:
# All disasters:
g.events <- disasters %>%
      filter(Year <= 2013) %>%
      group_by(Continent) %>%
      summarize(Events = length(Continent)) # The use of factors (i.e., Contenent) requires the use of length, not count, tally, or n.
g.deaths <- disasters %>%
      filter(Year <= 2013) %>%
      group_by(Continent) %>%
      summarize(Deaths = sum(`Total Deaths`, na.rm = TRUE))
g.affected <- disasters %>%
      filter(Year <= 2013) %>%
      group_by(Continent) %>%
      summarize(Affected = sum(`No Affected`, na.rm = TRUE))
g.damages <- disasters %>%
      filter(Year <= 2013) %>%
      group_by(Continent) %>%
      summarize(Affected = 1e-6 * sum(`Total Damages ('000 US$)`, na.rm = TRUE)) # NOTE: I have changed this to billions USD, it is no longer thousands of dollars.

# Just droughts:
drought <- filter(disasters, `Disaster Type` == "Drought") %>% arrange(Year)

# Begining with 1900-2013 to verify with example table: https://duq.box.com/shared/static/33r8abqvd8di5pre5lwmfb6uxrsdpfl3.png
# There are several mismatches: Americas - events, Africa and Asia - no. of deaths (Asia difference is big), all except Oceania - no. affected, Asia and Oceania - Damage
d.events <- drought %>%
      filter(Year <= 2013) %>%
      count(Continent) %>% # had used length, but this is more concise.  See: https://stat545.com/dplyr-single.html 
      arrange(as.character(Continent)) # the Continent list must be a character array, not a factor, to be alphabetized
d.deaths <- drought %>%
      filter(Year <= 2013) %>%
      group_by(Continent) %>%
      summarize(Deaths = sum(`Total Deaths`, na.rm = TRUE)) %>%
      arrange(as.character(Continent))
d.affected <- drought %>%
      filter(Year <= 2013) %>%
      group_by(Continent) %>%
      summarize(Affected = sum(`No Affected`, na.rm = TRUE)) %>%
      arrange(as.character(Continent))
d.damages <- drought %>%
      filter(Year <= 2013) %>%
      group_by(Continent) %>%
      summarize(Damages = 1e-6 * sum(`Total Damages ('000 US$)`, na.rm = TRUE)) %>% # NOTE: I have changed this to billions USD, it is no longer thousands of dollars.
      arrange(as.character(Continent))
d.sum <- data.frame(d.events$Continent, d.events$n, d.deaths$Deaths, d.affected$Affected, d.damages$Damages)
d.sum <- d.sum %>%
      rename(Continant = d.events.Continent, 
             `# of events` = d.events.n, 
             `# of people killed` = d.deaths.Deaths, 
             `# of people affected` = d.affected.Affected, 
             `Damage (Billions USD)` = d.damages.Damages)

# New table, with all data, 1900-2021
n.events <- drought %>%
      count(Continent) %>% # had used length, but this is more concise.  See: https://stat545.com/dplyr-single.html 
      arrange(as.character(Continent)) # the Continent list must be a character array, not a factor, to be alphabetized
n.deaths <- drought %>%
      group_by(Continent) %>%
      summarize(Deaths = sum(`Total Deaths`, na.rm = TRUE)) %>%
      arrange(as.character(Continent))
n.affected <- drought %>%
      group_by(Continent) %>%
      summarize(Affected = sum(as.numeric(`No Affected`), na.rm = TRUE)) %>%
      arrange(as.character(Continent))
n.damages <- drought %>%
      group_by(Continent) %>%
      summarize(Damages = 1e-6 * sum(`Total Damages ('000 US$)`, na.rm = TRUE)) %>% # NOTE: I have changed this to billions USD, it is no longer thousands of dollars.
      arrange(as.character(Continent))
n.sum <- data.frame(n.events$Continent, n.events$n, n.deaths$Deaths, n.affected$Affected, n.damages$Damages)
n.sum <- n.sum %>%
      rename(Continant = n.events.Continent, 
             `# of events` = n.events.n, 
             `# of people killed` = n.deaths.Deaths, 
             `# of people affected` = n.affected.Affected, 
             `Damage (Billions USD)` = n.damages.Damages)
# Brute force add a total row:
t.n.events <- sum(n.sum$`# of events`) # We tried to write a loop; however, it wasn't many columns and we would have to reassign the data type for each element for proper display.
t.n.deaths <- sum(n.sum$`# of people killed`)
t.n.affected <- sum(n.sum$`# of people affected`)
t.n.damage <- sum(n.sum$`Damage (Billions USD)`)
n.total <- data.frame("Total", t.n.events, t.n.deaths, t.n.affected, t.n.damage)
names(n.total) <- names(n.sum)
n.sum <- rbind(n.sum, n.total)
n.sum %>% 
      gt() %>% 
#      summary_rows(                    # This will add a total row, but it puts the total in a new colunn to the left of Continent: strange.
#            columns = c(2, 3, 4, 5), 
#            fns = list(Total = "sum")
#      ) %>%
      cols_align(
            align = "left", columns = 1
      ) %>% 
      cols_align(
            align = "right", columns = 2
      ) %>% 
      cols_align(
            align = "right", columns = 3
      ) %>% 
      cols_align(
            align = "right", columns = 4
      ) %>% 
      cols_align(
            align = "right", columns = 5
      ) %>% 
      tab_header(
            title = md("**Table 1.** Overview of droughts and their impact, 1900-2021") 
      ) %>% 
      fmt_number(columns = 5, decimals = 4) %>% 
      tab_source_note(md("*Data source:* EM-DAT: the International Disaster Database."))

# still on droughts
h <- hist(drought$Year, breaks = (1900.5:2021.5))
history <- data.frame(h$mids, h$counts)
ggplot(history, aes(x = h.mids, y = h.counts)) +
      geom_line() +
      labs(x = "Year", y = "Number of Global Droughts") +
      theme(panel.background = element_blank(), panel.border = element_rect(fill = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Let's look at southern Africa (REGIONAL)
droughtSA <- filter(disasters, `Disaster Type` == "Drought") %>% 
      filter(Region == "Southern Africa") %>%
      arrange(Year)
h <- hist(droughtSA$Year, breaks = (1900.5:2021.5))
historySA <- data.frame(h$mids, h$counts)
ggplot(historySA, aes(x = h.mids, y = h.counts)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(x = "Year", y = "Number of Droughts in Southern Africa") +
      theme(panel.background = element_blank(), panel.border = element_rect(fill = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Let's take a look at using dplyr to reduce these data:
sa.drought.count <- droughtSA %>% 
      count(Year) %>% 
      arrange(Year)
sa.drought.deaths <- droughtSA %>% 
      group_by(Year) %>%
      summarize(Deaths = sum(`Total Deaths`, na.rm = TRUE)) %>%
      arrange(Year)
sa.drought.affected <- droughtSA %>% 
      group_by(Year) %>%
      summarize(Affected = sum(`Total Affected`, na.rm = TRUE)) %>%
      arrange(Year)
sa.drought.damages <- droughtSA %>% 
      group_by(Year) %>%
      summarize(Damages = 1e3 * sum(`Total Damages ('000 US$)`, na.rm = TRUE)) %>% # This is now in USD
      arrange(Year)

# Short southern Africa summary table by year
sa.drought <- data.frame(sa.drought.count$Year, sa.drought.count$n, sa.drought.deaths$Deaths, sa.drought.affected$Affected, (sa.drought.damages$Damages/1e9))
sa.drought <- sa.drought %>%
      rename(Year = sa.drought.count.Year, 
             `# of events` = sa.drought.count.n, 
             `# of people killed` = sa.drought.deaths.Deaths, 
             `# of people affected` = sa.drought.affected.Affected, 
             `Damage (Billions USD)` = X.sa.drought.damages.Damages.1e.09.)
sa.drought %>% # mainly for review.
      gt() %>% 
      cols_align(
            align = "left", columns = 1
      ) %>% 
      cols_align(
            align = "right", columns = 2
      ) %>% 
      cols_align(
            align = "right", columns = 3
      ) %>% 
      cols_align(
            align = "right", columns = 4
      ) %>% 
      cols_align(
            align = "right", columns = 5
      ) %>% 
      tab_header(
            title = md("**Table 2a.** Effect of droughts in southern Africa, 1900-2021") 
      ) %>% 
      fmt_number(columns = 5, decimals = 4) %>% 
      tab_source_note(md("*Data source:* EM-DAT: the International Disaster Database."))

# Next, arrange in a table to plot
yr <- (1901:2021)
sa.drought.count.yr <- array(0, dim = c(4, length(yr)))
sa.drought.deaths.yr <- array(0, dim = length(yr))
sa.drought.affected.yr <- array(0, dim = length(yr))
sa.drought.damages.yr <- array(0, dim = length(yr))
for (i in 1:nrow(sa.drought.count)) {
      sa.drought.count.yr[(sa.drought.count$Year[i]-1900)] <- sa.drought.count$n[i]
}
for (i in 1:nrow(sa.drought.deaths)) { # These should be able to be run off the same loop; however, this is done in case there is a missing value somewhere that would cause a mismatch.
      sa.drought.deaths.yr[(sa.drought.deaths$Year[i]-1900)] <- sa.drought.deaths$Deaths[i]
}
for (i in 1:nrow(sa.drought.affected)) {
      sa.drought.affected.yr[(sa.drought.affected$Year[i]-1900)] <- sa.drought.affected$Affected[i]
}
for (i in 1:nrow(sa.drought.damages)) {
      sa.drought.damages.yr[(sa.drought.damages$Year[i]-1900)] <- (sa.drought.damages$Damages[i]/1e9) # Now, the values are presented as billion USD.  This is for plotting.
}

sa.drought.yr <- data.frame(yr,sa.drought.count.yr, sa.drought.deaths.yr, sa.drought.affected.yr, sa.drought.damages.yr)
sa.drought.yr <- sa.drought.yr %>%
      rename(Year = yr, 
             `# of events` = sa.drought.count.yr, 
             `# of people killed` = sa.drought.deaths.yr, 
             `# of people affected` = sa.drought.affected.yr, 
             `Damage (Billions USD)` = sa.drought.damages.yr)
sa.drought.yr %>% # mainly for review.
      gt() %>% 
      cols_align(
            align = "left", columns = 1
      ) %>% 
      cols_align(
            align = "right", columns = 2
      ) %>% 
      cols_align(
            align = "right", columns = 3
      ) %>% 
      cols_align(
            align = "right", columns = 4
      ) %>% 
      cols_align(
            align = "right", columns = 5
      ) %>% 
      tab_header(
            title = md("**Table 2.** Effect of droughts in southern Africa, 1900-2021") 
      ) %>% 
      fmt_number(columns = 5, decimals = 4) %>% 
      tab_source_note(md("*Data source:* EM-DAT: the International Disaster Database."))

# Renaming columns for plotting - ease of calling data
sa.drought.yr <- sa.drought.yr %>%
      rename(yr = Year, 
             n = `# of events`, 
             deaths = `# of people killed`, 
             affected = `# of people affected`, 
             damage9 = `Damage (Billions USD)`)
ggplot(sa.drought.yr, aes(x = yr, y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(x = "Year", y = "Number of Droughts in Southern Africa") +
      xlim(1950,2025) +
      theme(panel.background = element_blank(), panel.border = element_rect(fill = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggplot(sa.drought.yr, aes(x = yr, y = deaths)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(x = "Year", y = "Deaths from Droughts in Southern Africa") +
      xlim(1950,2025) +
      theme(panel.background = element_blank(), panel.border = element_rect(fill = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggplot(sa.drought.yr, aes(x = yr, y = (affected/1e6))) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(x = "Year", y = "Affected by Droughts in Southern Africa (Million people)") +
      xlim(1950,2025) +
      theme(panel.background = element_blank(), panel.border = element_rect(fill = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggplot(sa.drought.yr, aes(x = yr, y = damage9)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(x = "Year", y = "Damage from Droughts in Southern Africa (Billions USD)") +
      xlim(1950,2025) +
      theme(panel.background = element_blank(), panel.border = element_rect(fill = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

