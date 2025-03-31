# Figures for Bakar et al., (2005). Fluoride in drinking water and tea in the Arusha Region of Tanzania, Water.

library(beanplot)
library(ggplot2)
library(ggpattern)
library(viridis)
library(latex2exp)
library(dplyr)
library(tidyr)

####################
#                  #
#     FIGURE 1     #
#                  #
####################

pH <- c(8.70, 8.79, 8.66, 8.74, 8.46, 8.47, 8.47, 8.45, 8.51, 8.48, 8.52, 8.65, 8.65, 8.47, 8.64, 8.32, 8.37, 8.38, 8.39, 8.30, 8.42, 8.45, 8.40, 8.39, 8.54, 8.78)
cond <- c(681.7,706.2,676.9,681.7,561.5,552.8,550.9,556.3,555.4,553.4,554.8,673.6,688.4,550.6,684.1,571.7,569.2,562.6,560.5,556.6,555.9,569.9,566.4,573.1,705.1,707.1)
f <- c(3.22,4.01,4.08,4.14,2.76,2.86,2.56,2.77,2.64,2.82,2.82,2.78,4.00,4.04,2.80,3.60,2.74,2.75,2.74,2.76,2.78,2.77,2.74,2.76,4.02,4.18)

# par(mfrow = c(1,3), mar = c(5,4,2,2))
# beanplot(pH, xlab = "pH")
# beanplot(cond, xlab = "Conductivity", ylab = TeX('$\\mu$S/cm'))
# beanplot(f, xlab = "Fluoride", ylab = "mg/L")

par(mfrow = c(3,1), mar = c(5,5,2,2), cex.lab = 1.5)
pH <- data.frame(pH)
cond <- data.frame(cond)
f <- data.frame(f)
p1 <- ggplot(pH, aes(x = pH)) +
     geom_dotplot(stackdir='center', dotsize = 0.5) +
     xlab('pH') +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text = element_text(face = "plain", size = 12)) +
     theme(axis.title = element_text(face = "plain", size = 12)) +
     theme(axis.text.y = element_blank()) +
     theme(axis.title.y = element_blank()) +
     theme(axis.ticks.y = element_blank())
gp1 <- ggplotGrob(p1)
p2 <- ggplot(cond, aes(x = cond)) +
     geom_dotplot(stackdir='center', dotsize = 0.5) +
     xlab(TeX('Conductivity ($\\mu$S/cm)')) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text = element_text(face = "plain", size = 12)) +
     theme(axis.title = element_text(face = "plain", size = 12)) +
     theme(axis.text.y = element_blank()) +
     theme(axis.title.y = element_blank()) +
     theme(axis.ticks.y = element_blank())
gp2 <- ggplotGrob(p2)
p3 <- ggplot(f, aes(x = f)) +
     geom_dotplot(stackdir='center', dotsize = 0.5) +
     xlab('Fluoride (mg/L)') +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text = element_text(face = "plain", size = 12)) +
     theme(axis.title = element_text(face = "plain", size = 12)) +
     theme(axis.text.y = element_blank()) +
     theme(axis.title.y = element_blank()) +
     theme(axis.ticks.y = element_blank())
gp3 <- ggplotGrob(p3)
grid::grid.newpage()
physicochemical <- grid::grid.draw(rbind(gp1,gp2,gp3))

ecoli <- c(6,8,28,22,34,61,9,9,33,43,28,17,13,18,1,2,70,75)
tc <- c(190,216,42,54,300,300,69,62,173,135,300,300,277,224,168,104,300,300)

# par(mfrow = c(2,1), mar = c(5,4,2,2), cex.lab = 1)
# vioplot(ecoli, horizontal = TRUE, names = "E. coli")
# vioplot(tc, horizontal = TRUE, names = "Total Coliform", xlab = "CFU/100 mL")

####################
#                  #
#     FIGURE 2     #
#                  #
####################

water_levels <- c("0-0.5", "0.5-1", "1-1.5", "1.5-2", "2-2.5", "2.5-3")
water_consumed <- c(26, 13, 4, 1, 1, 1)
tea_levels <- c("0-0.2", "0.2-0.4", "0.4-0.6", "0.6-0.8", "0.8-1", "1-1.2")
tea_consumed <- c(6, 22, 11, 5, 1, 1)
consumption <- data.frame(water_levels, water_consumed, tea_levels, tea_consumed)
p1 <- ggplot(consumption) +
     geom_col(aes(x=water_levels, y=water_consumed), fill = "navy") +
     xlab("Drinking Water (L per capita)") +
     ylab("Household Responses") +
     ylim(c(0,30)) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text.x = element_text(angle=35, vjust=1, hjust=1)) +
     theme(axis.text = element_text(face = "plain", size = 14)) +
     theme(axis.title = element_text(face = "plain", size = 14))
gp1 <- ggplotGrob(p1)
p2 <- ggplot(consumption) +
     geom_col(aes(x=tea_levels, y=tea_consumed), fill = "navy") +
     xlab("Tea (L per capita)") +
     ylim(c(0,30)) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text.x = element_text(angle=35, vjust=1, hjust=1)) +
     theme(axis.text = element_text(face = "plain", size = 14)) +
     theme(axis.title = element_text(face = "plain", size = 14)) +
     theme(axis.text.y = element_blank()) +
     theme(axis.title.y = element_blank())
gp2 <- ggplotGrob(p2)
grid::grid.newpage()
water_tea <- grid::grid.draw(cbind(gp1,gp2))

####################
#                  #
#     FIGURE 3     #
#                  #
####################

tea <- factor(c(rep("Tanzania Green (loose)", 3), rep("Kilimanjaro Black (bag)", 3), rep("Bigelow Oolong (bag)", 3)), levels = c("Tanzania Green (loose)", "Kilimanjaro Black (bag)", "Bigelow Oolong (bag)"))
Water <- factor(c(rep(c("local", "DI", "SGW"), 3)), levels = c("local", "DI", "SGW"))
value <- c(6.34, 3.23, 4.48, 0, 2.67, 3.28, 0, 2.81, 2.68)
se <- c(0.21, 0.06, 0.07, 0, 0.15, 0.01, 0, 0.12, 0.01)
Ftea <- data.frame(tea,Water,value,se)
ggplot(Ftea, aes(tea, y = value, fill = Water)) +
     geom_col(color = "black", position = "dodge") +
     scale_fill_viridis(discrete = TRUE) +
     geom_errorbar(aes(ymin = value-se, ymax = value+se), width = 0.2, position = position_dodge(0.9)) +
     ylab("Fluoride (mg/L)") +
     ylim(c(0,7)) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text = element_text(face = "plain", size = 14)) +
     theme(axis.title = element_text(face = "plain", size = 14)) +
     theme(legend.title = element_text(face = "plain", size = 14)) +
     theme(legend.text = element_text(face = "plain", size = 14)) +
     theme(axis.text.x = element_text(angle=25, vjust=0.7, hjust=0.7)) +
     theme(axis.title.x = element_blank())

####################
#                  #
#     FIGURE 4     #
#                  #
####################

tea <- factor(c(rep("Tanzania Green (loose)", 5), rep("Kilimanjaro Black (bag)", 5), rep("Bigelow Oolong (bag)", 5)), levels = c("Tanzania Green (loose)", "Kilimanjaro Black (bag)", "Bigelow Oolong (bag)"))
Milk <- factor(c(rep(c("Brewed with commercial", "Commercial", "Cow", "Goat", "Cow+goat"), 3)), levels = c("Brewed with commercial", "Commercial", "Cow", "Goat", "Cow+goat"))
value <- c(1.90, 1.86, 0, 0, 0, 1.18, 2.20, 2.14, 2.00, 2.20, 1.54, 1.82, 1.97, 2.00, 1.92)
se <- c(0.03, 0.20, 0, 0, 0, 0.02, 0.03, 0.01, 0.02, 0.03, 0.01, 0.04, 0.01, 0.00, 0.01)
Fmilk <- data.frame(tea,Milk,value,se)
ggplot(Fmilk, aes(tea, y = value, fill = Milk)) +
     geom_col(color = "black", position = "dodge") +
     scale_fill_viridis(discrete = TRUE) +
     geom_errorbar(aes(ymin = value-se, ymax = value+se), width = 0.2, position = position_dodge(0.9)) +
     ylab("Fluoride (mg/L)") +
     ylim(c(0,2.5)) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text = element_text(face = "plain", size = 14)) +
     theme(axis.title = element_text(face = "plain", size = 14)) +
     theme(legend.title = element_text(face = "plain", size = 14)) +
     theme(legend.text = element_text(face = "plain", size = 14)) +
     theme(axis.text.x = element_text(angle=25, vjust=0.7, hjust=0.7)) +
     theme(axis.title.x = element_blank())

####################
#                  #
#     FIGURE 5     #
#                  #
####################

value <- c(-0.67, -0.71, 0, 0, 0, -0.79, 0.23, 0.16, 0.02, 0.22, -0.13, 0.15, 0.29, 0.32, 0.24)
missing <- data.frame(tea,Milk,value)
ggplot(missing, aes(tea, y = value, fill = Milk)) +
     geom_col(color = "black", position = "dodge") +
     scale_fill_viridis(discrete = TRUE) +
     ylab("Fluoride (mg/L)") +
     scale_y_continuous(breaks=seq(-0.8,0.4,0.2)) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text = element_text(face = "plain", size = 14)) +
     theme(axis.title = element_text(face = "plain", size = 14)) +
     theme(legend.title = element_text(face = "plain", size = 14)) +
     theme(legend.text = element_text(face = "plain", size = 14)) +
     theme(axis.text.x = element_text(angle=25, vjust=0.7, hjust=0.7)) +
     theme(axis.title.x = element_blank())


## REFERENCES!
`Reference Years` <- c(2015, 2012, 2006, 2017, 2020, 2022, 2017, 2020, 2022, 2024, 2020, 2002, 2016, 2008, 2007, 2010, 2020, 2013, 2003, 2001, 2007, 2007, 2009, 1999, 2008, 2021, 2002, 2018, 2012)
# excluding data and personal communications
h <- hist(`Reference Years`)
ggplot(h) +
     geom_col(aes(x=water_levels, y=water_consumed), fill = "navy") +
     xlab("Drinking Water (L per capita)") +
     ylab("Household Responses") +
     ylim(c(0,30)) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text.x = element_text(angle=35, vjust=1, hjust=1)) +
     theme(axis.text = element_text(face = "plain", size = 14)) +
     theme(axis.title = element_text(face = "plain", size = 14))





