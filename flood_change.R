library(readr)
library(devtools)
library(hydrostats)

dat <- read_tsv("usgs03049800.tsv", skip = 72)
dat$ln_peak <- log(dat$peak_va, base = 10)
first <- dat[1:30,]
second <- dat[31:60,]

m1 <- mean(first$ln_peak)
std1 <- stdev(first$ln_peak)
c3_1 <- skew(first$ln_peak)
lp3 <- pt3(c3_1,0.99)
flood1 <- m1+std1*10^(lp3)

m2 <- mean(second$ln_peak)
std2 <- stdev(second$ln_peak)
c3_2 <- skew(second$ln_peak)
lp3 <- pt3(c3_2,0.99)
flood2 <- m2+std2*10^(lp3)
