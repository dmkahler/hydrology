# To pull and organize turbidity data from the Turner Designs sonde
library(tidyverse)

# file from sonde has headers at line 6 and data starts at line 8
header = c("unix_time", "UTC", "EST", "battery", "temperature", "turbidity", "gain")
turb <- read_csv("2020Jan_turbidity.TXT", skip = 7, col_names = header)

# Need to convert time to South Africa Standard time, UTC+2, or 7200 seconds.  SAST and CAT are not valid time zones 
# and the European time zones change with daylight savings.  This is hard-coded with two hours added.
SAST <- as.POSIXlt((turb$unix_time+7200), origin = "1970-01-01", tz = "UTC") # ACHTUNG! THIS IS NOT UTC, this is SAST
year <- as.numeric(format(SAST, '%Y'))
mont <- as.numeric(format(SAST, '%m'))
dayn <- as.numeric(format(SAST, '%d'))
hour <- as.numeric(format(SAST, '%H'))
minu <- as.numeric(format(SAST, '%M'))

turbQC_lo <- 0 # NTU, lower limit
turbQC_hi <- 1000 # NTU, upper limit
QCflag <- array(0, dim = nrow(turb))
turb_corrected <- array(-9999, dim = nrow(turb))
for (i in 1:nrow(turb)) {
      turb_corrected[i] <- turb$turbidity[i]
      if (i > 3) {
            if ((turb$turbidity[i] == turb$turbidity[(i-1)]) && (turb$turbidity[i] == turb$turbidity[(i-2)])) {
                  QCflag[i] <- QCflag[i] + 5 # possible stuck value
            }
      }
      if ((i > 1) && (i < nrow(turb))) {
            # Seeking to qualify any results that show a spike or drop
            if ( ((turb$turbidity[i]>turb$turbidity[i-1]) && (turb$turbidity[i]>turb$turbidity[i+1])) || ((turb$turbidity[i]<turb$turbidity[i-1])&&(turb$turbidity[i]<turb$turbidity[i+1])) ) {    
                  dturb1 <- abs((turb$turbidity[i]-turb$turbidity[i-1])/turb$turbidity[i-1])
                  dturb2 <- abs((turb$turbidity[i+1]-turb$turbidity[i])/turb$turbidity[i])
                  if ((dturb1 > 0.20) && (dturb2 > 0.20)) {
                        QCflag[i] <- QCflag[i] + 1 # possible spike
                        turb_corrected[i] <- (turb$turbidity[i-1]+turb$turbidity[i+1])/2
                  }
            }
      }
      if (turb$turbidity[i] > turbQC_hi) {
            QCflag[i] <- QCflag[i] + 2 # unusually high value
            turb_corrected[i] <- turbQC_hi
      }
      if (turb$turbidity[i] < turbQC_lo) {
            QCflag[i] <- QCflag[i] + 4 # unusually low value
            turb_corrected[i] <- turbQC_lo
      }
}
plot(SAST,QCflag, ylim = c(0, 7), xlab = "Time (local, SAST)", ylab = "QC Codes")
plot(SAST,turb$turbidity, ylim = c(turbQC_lo,turbQC_hi), xlab = "Time (local, SAST)", ylab = "Turbidity (NTU)")

output <- data.frame(turb$unix_time, year, mont, dayn, hour, minu, turb$turbidity, QCflag, turb_corrected)
write.table(output, "turbidity.csv", sep = ", ", dec = ".", row.names = FALSE)
