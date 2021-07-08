library(beanplot)
library(vioplot)
library(latex2exp)

pH <- c(8.70, 8.79, 8.66, 8.74, 8.46, 8.47, 8.47, 8.45, 8.51, 8.48, 8.52, 8.65, 8.65, 8.47, 8.64, 8.32, 8.37, 8.38, 8.39, 8.30, 8.42, 8.45, 8.40, 8.39, 8.54, 8.78)
cond <- c(681.7,706.2,676.9,681.7,561.5,552.8,550.9,556.3,555.4,553.4,554.8,673.6,688.4,550.6,684.1,571.7,569.2,562.6,560.5,556.6,555.9,569.9,566.4,573.1,705.1,707.1)
f <- c(3.22,4.01,4.08,4.14,2.76,2.86,2.56,2.77,2.64,2.82,2.82,2.78,4.00,4.04,2.80,3.60,2.74,2.75,2.74,2.76,2.78,2.77,2.74,2.76,4.02,4.18)

par(mfrow = c(1,3), mar = c(5,4,2,2))
beanplot(pH, xlab = "pH")
beanplot(cond, xlab = "Conductivity", ylab = TeX('$\\mu$S/cm'))
beanplot(f, xlab = "Fluoride", ylab = "mg/L")

par(mfrow = c(3,1), mar = c(5,5,2,2), cex.lab = 1.5)
vioplot(pH, horizontal = TRUE, cex.names = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, cex.sub = 1.5, names = "pH")
vioplot(cond, horizontal = TRUE, cex.names = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, cex.sub = 1.5, names = "Conductivity", xlab = TeX('$\\mu$S/cm'))
vioplot(f, horizontal = TRUE, cex.names = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, cex.sub = 1.5, names = "Fluoride", xlab = "mg/L")

ecoli <- c(6,8,28,22,34,61,9,9,33,43,28,17,13,18,1,2,70,75)
tc <- c(190,216,42,54,300,300,69,62,173,135,300,300,277,224,168,104,300,300)

par(mfrow = c(2,1), mar = c(5,4,2,2), cex.lab = 1)
vioplot(ecoli, horizontal = TRUE, names = "E. coli")
vioplot(tc, horizontal = TRUE, names = "Total Coliform", xlab = "CFU/100 mL")


