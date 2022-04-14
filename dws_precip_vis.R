# To visualize precip data

library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(latex2exp)
# install_github("LimpopoLab/hydrostats")
library(hydrostats)

gpm <- read_csv("GPM_Musina.csv", skip = 8)
gpm <- gpm %>%
     rename(dt = time,precip = mean_GPM_3IMERGDF_06_precipitationCal)

gpm_ann <- gpm %>%
     mutate(hydro.yr = hyd.yr(dt, h = "S")) %>%
     group_by(hydro.yr) %>%
     summarize(ann.p = sum(precip, na.rm = TRUE))

gpm_ann_ave <- mean(gpm_ann$ann.p, na.rm = TRUE)
# average annual precip is 413 mm

ann_precip <- ggplot(gpm_ann) +
     geom_col(aes(x=hydro.yr,y=ann.p)) +
     geom_hline(yintercept = gpm_ann_ave) +
     xlim(c(2000,2022)) +
     ylim(c(0,700)) +
     xlab("Hydrologic Year") + 
     ylab("Annual Precipitation (mm)") + 
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(legend.position="right") + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12))
ggsave("annual_precip.eps", ann_precip, device = "eps", dpi = 72)

