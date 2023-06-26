library(readr)
library(dplyr)
library(ggplot2)

dat <- read_csv("UV_results.csv", skip = 1) %>% # /Users/davidkahler/Documents/PureThirst/clinic/
     rename(`Percent Removal`=percent)

dat$`Log Reduction` <- NA
for (i in 1:nrow(dat)) {
     if (dat$test[i] == 0) {
          dat$test[i] <- 0.5
     }
     dat$`Log Reduction`[i] <- -log10(dat$test[i]/dat$control[i])
}

ggplot(dat) +
     geom_point(aes(x=exp,y=W,color=`Log Reduction`),size=6) +
     xlab("Exposure Time (s)") +
     ylab("Power (W)") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 14)) +
     theme(axis.title = element_text(face = "plain", size = 14))
