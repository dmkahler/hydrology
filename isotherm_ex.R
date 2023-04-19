library(readr)
library(dplyr)
library(ggplot2)
library(latex2exp)

x <- read_csv("CaAugmentation.csv")

Cl2 <- filter(x, Augmentation=="CaCl2")
CO3 <- filter(x, Augmentation=="CaCO3")

ggplot(Cl2) +
     geom_point(aes(x=C,y=`C*`), size=3) +
     xlab("Equilibrium Concentration (mg/l)") + 
     ylab("Sorbed Mass (mg/g)") + 
     xlim(c(0,8)) +
     ylim(c(0,1.5)) + 
     theme(aspect.ratio = 1, 
           panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text = element_text(face = "plain", size = 14), 
           axis.title = element_text(face = "plain", size = 14))

ggplot(CO3) +
     geom_point(aes(x=C,y=`C*`), size=3) +
     xlab("Equilibrium Concentration (mg/l)") + 
     ylab("Sorbed Mass (mg/g)") + 
     xlim(c(0,2)) +
     ylim(c(0,10)) + 
     theme(aspect.ratio = 1, 
           panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text = element_text(face = "plain", size = 14), 
           axis.title = element_text(face = "plain", size = 14))

ggplot(x) +
     geom_point(aes(x=C,y=`C*`,shape=Augmentation), size=6) +
     scale_shape_manual(values=c(1,5), labels=c(expression(CaCl[2],CaCO[3])) ) +
     xlab("Equilibrium Concentration (mg/l)") + 
     ylab("Sorbed Mass (mg/g)") + 
     theme(aspect.ratio = 1, 
           panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text = element_text(face = "plain", size = 18), 
           axis.title = element_text(face = "plain", size = 18)) +
     theme(legend.position="right", 
           legend.key = element_blank(), 
           legend.text = element_text(face = "plain", size = 16), 
           legend.title = element_text(face = "plain", size = 16)) 



