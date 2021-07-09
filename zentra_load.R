# This code is to download data from ZentraCloud and arrange it in csv format.
# PASSWORDS HAVE BEEN REMOVED FROM THIS VERSION.  IT WILL NOT FUNCTION

# Site      Device      Password
# suggested list here.

# Select station on lines 39-42, preallocate arrays on lines 129-136, and activate analysis on lines 337

# CUAHSI hosts a data repository and uses the package WaterML to interact with that repository
# https://github.com/jirikadlec2/waterml
# https://www.cuahsi.org/data-models/publication/
# install.packages("WaterML")
# library(WaterML)
# http://hydroserver.cuahsi.org/home/index

# Recommended API method from:
# https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/API-data-access-r/
# library(ggmap)
library(ggplot2)
library(dplyr)
library(rjson)
# library(jsonlite)
library(RCurl)

# The API information for Zentra Cloud is tied to my account and the device serial number&password (orange labels)
# https://sites.google.com/metergroup.com/zentracloudwiki/home/how-to-main-menu/api?authuser=0
# https://zentracloud.com/api/v1/guide
# WITH TOKEN
# {
#       "token": ""
# }
# https://zentracloud.com/api/v1/readings?sn=z6-02386&start_mrid=13
# WITH USERNAME AND PASSWORD
# https://zentracloud.com/api/v1/readings?user=USERNAME&user_password=PASSWORD&sn=SERIAL&device_password=PASSWORD&start_mrid=STARTINGPOINT

# Assemble API endpoint to pull data from ZentraCloud
base_url = "https://zentracloud.com/api/v1/readings"
user_url = "?user=<email address goes here>&user_password=<password goes here>"
sttn_url = "&sn=<device number>&device_password=<device password, from sticker>"  # Note site here for multiple site use
# sttn_url = "&sn=<___>&device_password=<___>"  # 
mrid <- 12  # This is the last mrid loaded, may pull this information from mrid.csv or HydroClient
strt_url = paste0("&start_mrid=", (mrid+1), "")
full_url = paste0(base_url, user_url, sttn_url, strt_url, "")
api_end <- URLencode(full_url)

# Query API and save snapshot
# Still following earthsciencedata.com, but found syntax for getURL and allowance to avoid SSL requirement:
# https://www.rdocumentation.org/packages/RCurl/versions/1.98-1.2/topics/getURL
# https://www.r-bloggers.com/a-tiny-rcurl-headache/
hydromet <- fromJSON(getURL(api_end, ssl.verifypeer = FALSE))
# Save data to JSON data structure for backup -- TWO OPTIONS FOR WORKING DIRECTORY
setwd("")
# setwd("")
site <- "Olkokola"
today <- Sys.Date()
exportJSON <- toJSON(hydromet)
save(exportJSON, file = paste0(site, "_", today, ".JSON", ""))
rm(exportJSON)

# Check data download
tracker <- array(-99, dim=c(1,6))
tracker[1,1] <- toString(as.POSIXlt(Sys.time(), "GMT")) # the current/download time in UTC
tracker[1,2] <- (mrid+1) # record start reference
begin <- hydromet$device$timeseries[[1]]$configuration$values[[1]][[1]] # first date/time in download, in seconds from 1970-01-01
tracker[1,3] <- toString(as.POSIXct(begin), origin = "1970-01-01", tz = "GMT") # first date/time in download, in date format
num_val <- length(hydromet$device$timeseries[[1]]$configuration$values) # number of recordings
tracker[1,4] <- num_val
endd <- hydromet$device$timeseries[[1]]$configuration$values[[num_val]][[1]] # last date/time in download, in seconds from 1970-01-01
tracker[1,5] <- toString(as.POSIXct(endd), origin = "1970-01-01", tz = "GMT") # last date/time in download, in date format
tracker[1,6] <- (mrid+num_val)
# headers: NUM,DOWNLOAD_DATE_TIME,START_MRID,BEGIN_DATE_TIME,NUMBER_OF_RECORDS,LAST_MRID
write.table(tracker, file = paste0(site, "_mrid.csv", ""), append = TRUE, sep = ",", dec = ".", col.names = FALSE)
# Check number of recordings:
print(paste0("Number of recordings: ", num_val, sep = ""))
print(paste0("Number of time steps (inclusive): ", ((dur/900)+1)), sep = "") # 900 is # of seconds in a 15 minute interval, the plus one is to include the beginning step
print("Check that these values are the same")
print(paste0("End date and time: ", tracker[1,5], sep = "")) # end date/time in record
print(paste0("Current date and time: ", tracker[1,1], sep = "")) # current date/time in record
print("Check that the end time is not more than 24 hours before the current time")

# Sort data
# hydromet$device$timeseries[[1]]$configuration$values[[i]][[1]][[1]]          Date&time                (UTC seconds) from 1970-01-01
# hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[1]]$value    Solar Radiation          (W/m^2)
# hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[2]]$value    Precipitation            (mm)
# hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[3]]$value    Lighting Activity        (count)
# hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[4]]$value    Lighting Distance        (km)
# hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[5]]$value    Wind Direction           (degrees)
# hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[6]]$value    Wind Speed               (m/s)
# hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[7]]$value    Gust Speed               (m/s)
# hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[8]]$value    Air Temperature          (degrees C)
# hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[9]]$value    Vapor Pressure           (kPa)
# hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[10]]$value   Atmospheric Pressure     (kPa)
# hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[11]]$value   X-axis Level             (degrees) (0 is vertical)
# hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[12]]$value   Y-axis Level             (degrees)
# hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[13]]$value   Max Precip Rate          (mm/h)
# hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[14]]$value   RH Sensor Temp           (degrees C)
# hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[15]]$value   Vapor Pressure Deficit   (kPa)
# hydromet$device$timeseries[[1]]$configuration$values[[i]][[5]][[1]]$value    Battery Percent          (%)
# hydromet$device$timeseries[[1]]$configuration$values[[i]][[5]][[2]]$value    Battery Voltage          (mV)
# hydromet$device$timeseries[[1]]$configuration$values[[i]][[6]][[1]]$value    Reference Pressure       (kPa)
# hydromet$device$timeseries[[1]]$configuration$values[[i]][[6]][[2]]$value    Logger Temperature       (degrees C)
# $description   text
# $value         number
# $unit          text
# $error         logical: TRUE or FALSE
# Preallocation:
DATE <- array(-9999, dim = c(num_val,1)) # date and time in seconds from 00:00 01 Jan 1970, UTC; local time is SAST (UTC+2) or EAT (UTC+3)
YEAR <- DATE # year
MNTH <- YEAR # month
DAYN <- YEAR # day of month
HOUR <- YEAR # hour
MINU <- YEAR # minute
PRCP <- YEAR # precipitation (mm)
PRCPqc <- array(0, dim = c(num_val,1))
SRAD <- YEAR # solar radiation (W/m^2)
SRADqc <- PRCPqc
TEMP <- YEAR # air temperature (degrees C)
TEMPqc <- PRCPqc
RHMD <- YEAR # relative humidity (%)
RHMDqc <- PRCPqc
APRS <- YEAR # air pressure (kPa)
APRSqc <- PRCPqc
WSPD <- YEAR # wind speed (m/s)
WSPDqc <- PRCPqc
WDIR <- YEAR # wind direction (degrees)
WDIRqc <- PRCPqc
#RIVS <- YEAR
#RIVSqc <- PRCPqc
#WTMP <- YEAR
#WTMPqc <- PRCPqc
#COND <- YEAR
#CONDqc <- PRCPqc
#TRBD <- YEAR
#TRBDqc <- PRCPqc
for (i in 560:num_val) {
      DATE[i,1] <- hydromet$device$timeseries[[1]]$configuration$values[[i]][[1]][[1]]
      dt1 <- as.POSIXlt(DATE[i,1], origin = "1970-01-01", tz = "GMT")
      dt2 <- as.Date(dt1, '%Y/%m/%d')
      YEAR[i,1] <- as.numeric(format(dt2, '%Y'))
      MNTH[i,1] <- as.numeric(format(dt2, '%m'))
      DAYN[i,1] <- as.numeric(format(dt2, '%d'))
      HOUR[i,1] <- dt1$hour
      MINU[i,1] <- dt1$min
      # SOLAR RADIATION
      val <- hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[1]]$value
      if (is.numeric(val) == TRUE) {
            SRAD[i,1] <- val
            error <- hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[1]]$error
            if (error == FALSE) {
                  SRADqc[i,1] <- 1
                  if (i>6) {
                        if (SRAD[i,1] == SRAD[i-1,1]) {
                              if (SRAD[i,1] == SRAD[i-2,1]) {
                                    if (SRAD[i,1] == SRAD[i-3,1]) {
                                          if (SRAD[i,1] == SRAD[i-4,1]) {
                                                if (SRAD[i,1] == SRAD[i-5,1]) {
                                                      SRADqc[i,1] <- SRADqc[i,1] + 100 # detects and indicates a stuck value
                                                }
                                          }
                                    }
                              }
                        }
                  }
                  if (SRAD[i,1] < 0) {
                        SRADqc[i,1] <- SRADqc[i,1] + 10 # detects and indicates a value below range
                  }
                  if (SRAD[i,1] > 2000) {
                        SRADqc[i,1] <- SRADqc[i,1] + 20 # detects and indicates a value above range
                  }
            } else {
                  SRAD[i,1]  <- -7777
                  SRADqc[i,1] <- 900 # marks an error passed from the datalogger
            }
      } else {
            SRAD[i,1] <- -9999
            SRADqc[i,1] <- 90
      }
      # PRECIPITATION
      val <- hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[2]]$value
      if (is.numeric(val) == TRUE) {
            PRCP[i,1] <- val
            error <- hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[2]]$error
            if (error == FALSE) {
                  PRCPqc[i,1] <- 1
                  if (i>6) {
                        if (PRCP[i,1] > 0) { # exempts repetative value error if zero.
                              if (PRCP[i,1] == PRCP[i-1,1]) {
                                    if (PRCP[i,1] == PRCP[i-2,1]) {
                                          if (PRCP[i,1] == PRCP[i-3,1]) {
                                                if (PRCP[i,1] == PRCP[i-4,1]) {
                                                      if (PRCP[i,1] == PRCP[i-5,1]) {
                                                            PRCPqc[i,1] <- PRCPqc[i,1] + 100 # detects and indicates a stuck value
                                                      }
                                                }
                                          }
                                    }
                              }
                        }
                  }
                  if (PRCP[i,1] < 0) {
                        PRCPqc[i,1] <- PRCPqc[i,1] + 10 # detects and indicates a value below range
                  }
                  if (PRCP[i,1] > 2000) {
                        PRCPqc[i,1] <- PRCPqc[i,1] + 20 # detects and indicates a value above range
                  }
            } else {
                  PRCP[i,1]  <- -7777
                  PRCPqc[i,1] <- 900 # marks an error passed from the datalogger
            }
      } else {
            PRCP[i,1] <- -9999
            PRCPqc[i,1] <- 90
      }
      # WIND DIRECTION
      val <- hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[5]]$value
      if (is.numeric(val) == TRUE) {
            WDIR[i,1] <- val
            error <- hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[5]]$error
            if (error == FALSE) { # ***no stuck value detection for wind direction
                  WDIRqc[i,1] <- 1
                  if (WDIR[i,1] < 0) {
                        WDIRqc[i,1] <- 10 # detects and indicates a value below range
                  }
                  if (WDIR[i,1] > 360) {
                        WDIRqc[i,1] <- 20 # detects and indicates a value above range
                  }
            } else {
                  WDIR[i,1] <- -7777
                  WDIRqc[i,1] <- 900 # marks an error passed from the datalogger
            }
      } else {
            WDIR[i,1] <- -9999
            WDIR[i,1] <- 90
      }
      # WIND SPEED
      val <- hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[6]]$value
      if (is.numeric(val) == TRUE) {
            WSPD[i,1] <- val
            error <- hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[6]]$error
            if (error == FALSE) {
                  WSPDqc[i,1] <- 1
                  if (i>6) {
                        if (WSPD[i,1] == WSPD[i-1,1]) {
                              if (WSPD[i,1] == WSPD[i-2,1]) {
                                    if (WSPD[i,1] == WSPD[i-3,1]) {
                                          if (WSPD[i,1] == WSPD[i-4,1]) {
                                                if (WSPD[i,1] == WSPD[i-5,1]) {
                                                      WSPDqc[i,1] <- WSPDqc[i,1] + 100 # detects and indicates a stuck value
                                                }
                                          }
                                    }
                              }
                        }
                  }
                  if (WSPD[i,1] < 0) {
                        WSPDqc[i,1] <- WSPDqc[i,1] + 10 # detects and indicates a value below range
                  }
                  if (WSPD[i,1] > 100) {
                        WSPDqc[i,1] <- WSPDqc[i,1] + 20 # detects and indicates a value above range
                  }
            } else {
                  WSPD[i,1] <- -7777
                  WSPDqc[i,1] <- 900 # marks an error passed from the datalogger
            }
      } else {
            WSPD[i,1] <- -9999
            WSPDqc[i,1] <- 90
      }
      # AIR TEMPERATURE
      val <- hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[8]]$value
      if (is.numeric(val) == TRUE) {
            TEMP[i,1] <- val
            error <- hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[8]]$error
            if (error == FALSE) {
                  TEMPqc[i,1] <- 1
                  if (i>6) {
                        if (TEMP[i,1] == TEMP[i-1,1]) {
                              if (TEMP[i,1] == TEMP[i-2,1]) {
                                    if (TEMP[i,1] == TEMP[i-3,1]) {
                                          if (TEMP[i,1] == TEMP[i-4,1]) {
                                                if (TEMP[i,1] == TEMP[i-5,1]) {
                                                      TEMPqc[i,1] <- TEMPqc[i,1] + 100 # detects and indicates a stuck value
                                                }
                                          }
                                    }
                              }
                        }
                  }
                  if (TEMP[i,1] < 0) {
                        TEMPqc[i,1] <- TEMPqc[i,1] + 10 # detects and indicates a value below range
                  }
                  if (TEMP[i,1] > 50) {
                        TEMPqc[i,1] <- TEMPqc[i,1] + 20 # detects and indicates a value above range
                  }
            } else {
                  TEMP[i,1] <- -7777
                  TEMPqc[i,1] <- 900 # marks an error passed from the datalogger
            }
      } else {
            TEMP[i,1] <- -9999
            TEMPqc[i,1] <- 90
      }
      # RELATIVE HUMIDITY
      if (abs(TEMPqc[i,1]) < 10) { # will still calculate if it is a possible stuck error
            at <- TEMP[i,1] + 273.15 # convert air temperature to Kelvin
            svp <- (6984.505294+at*(-188.903931+at*(2.133357675+at*(-0.01288580973+at*(0.00004393587233+at*(-0.00000008023923082+at*6.136820929E-11))))))/10 # compute saturation vapor pressure in kPa via the Goff-Gratch equation, in nested form (Lowe, 1977; Brutsaert, 2005)
            val <- hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[9]]$value # import vapor pressure in kPa
            if (is.numeric(val) == TRUE) {
                  vp <- val
                  RHMD[i,1] <- round(100*(vp/svp), digits = 1) # compute relative humidity in %
                  error <- hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[9]]$error
                  if (error == FALSE) {
                        RHMDqc[i,1] <- 2
                        if (i>6) {
                              if (RHMD[i,1] == RHMD[i-1,1]) {
                                    if (RHMD[i,1] == RHMD[i-2,1]) {
                                          if (RHMD[i,1] == RHMD[i-3,1]) {
                                                if (RHMD[i,1] == RHMD[i-4,1]) {
                                                      if (RHMD[i,1] == RHMD[i-5,1]) {
                                                            RHMDqc[i,1] <- RHMDqc[i,1] + 100 # detects and indicates a stuck value
                                                      }
                                                }
                                          }
                                    }
                              }
                        }
                        if (RHMD[i,1] < 0) {
                              RHMDqc[i,1] <- RHMDqc[i,1] + 10 # detects and indicates a value below range
                        }
                        if (RHMD[i,1] > 100) {
                              RHMDqc[i,1] <- RHMDqc[i,1] + 20 # detects and indicates a value above range
                        }
                        if (TEMPqc[i,1] > 5) {
                              RHMDqc[i,1] <- RHMDqc[i,1] + 30 # marks any problem based on air temperature error
                        }
                  } else {
                        RHMD[i,1] <- -7777
                        RHMDqc[i,1] <- 900 # marks an error passed from the datalogger
                  }
                  
            } else {
                  RHMD[i,1] <- -9999
                  RHMDqc[i,1] <- 90
            }
      } else {
            RHMD[i,1] <- -7777
            RHMDqc[i,1] <- 80
      }
      # AIR PRESSURE
      val <- hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[10]]$value
      if (is.numeric(val) == TRUE) {
            APRS[i,1] <- val
            error <- hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[10]]$error
            if (error == FALSE) {
                  APRSqc[i,1] <- 1
                  if (i>6) {
                        if (APRS[i,1] == APRS[i-1]) {
                              if (APRS[i,1] == APRS[i-2]) {
                                    if (APRS[i,1] == APRS[i-3]) {
                                          if (APRS[i,1] == APRS[i-4]) {
                                                if (APRS[i,1] == APRS[i-5]) {
                                                      APRSqc[i,1] <- APRSqc[i,1] + 100 # detects and indicates a stuck value
                                                }
                                          }
                                    }
                              }
                        }
                  }
                  if (APRS[i,1] < 70) {
                        APRSqc[i,1] <- APRSqc[i,1] + 10 # detects and indicates a value below range
                  }
                  if (APRS[i,1] > 100) {
                        APRSqc[i,1] <- APRSqc[i,1] + 20 # detects and indicates a value above range
                  }
            } else {
                  APRS[i,1] <- -7777
                  APRSqc[i,1] <- 900 # marks an error passed from the datalogger
            }
      } else {
            APRS[i,1] <- -9999
            APRSqc[i,1] <- 90
      }
      x <- hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[11]]$value
      y <- hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[12]]$value
      if (is.numeric(x) == TRUE) {
            if (is.numeric(y) == TRUE) {
                  tilt <- sqrt((x^2)+(y^2))
                  if (tilt >= 2) {
                        PRCPqc[i,1] <- PRCPqc[i,1] + 1000
                        SRADqc[i,1] <- SRADqc[i,1] + 1000
                        WSPDqc[i,1] <- WSPDqc[i,1] + 1000
                        WDIRqc[i,1] <- WDIRqc[i,1] + 1000
                        print(paste0("Sensors tilted past 2 degrees ", dt1, sep = "")) # error time
                  }
            } else {
                  PRCPqc[i,1] <- PRCPqc[i,1] + 2000
                  SRADqc[i,1] <- SRADqc[i,1] + 2000
                  WSPDqc[i,1] <- WSPDqc[i,1] + 2000
                  WDIRqc[i,1] <- WDIRqc[i,1] + 2000
            }
      } else {
            PRCPqc[i,1] <- PRCPqc[i,1] + 2000
            SRADqc[i,1] <- SRADqc[i,1] + 2000
            WSPDqc[i,1] <- WSPDqc[i,1] + 2000
            WDIRqc[i,1] <- WDIRqc[i,1] + 2000
      }
}
# QC flags (in XXXXqc fields):
# xxx0 Raw data
# xxx1 Edited data
# xxx2 Derived value
# xxx3 Interpreted value
# xxx4 Knowledge product
# 000x No concerns
# xx1x Value below range
# xx2x Value above range
# xx3x, xx4x, xx5x, Dependent sensor out of QC
# xx9x Non-numeric value generated
# x1xx Possible stuck value
# x9xx Error passed from datalogger
# 1xxx Tilt of sensor is out of QC range (2 degrees)
# 2xxx Tilt data missing

# Store to data frame and export
df <- data.frame(DATE, YEAR, MNTH, DAYN, HOUR, MINU, PRCP, PRCPqc, SRAD, SRADqc, TEMP, TEMPqc, RHMD, RHMDqc, APRS, APRSqc, WSPD, WSPDqc, WDIR, WDIRqc)
#df <- data.frame(DATE, YEAR, MNTH, DAYN, HOUR, MINU, PRCP, PRCPqc, SRAD, SRADqc, TEMP, TEMPqc, RHMD, RHMDqc, APRS, APRSqc, WSPD, WSPDqc, WDIR, WDIRqc, RIVS, RIVSqc, WTMP, WTMPqc, COND, CONDqc, TRBD, TRBDqc)
write.table(df, file = paste0(site, "_", today, ".csv", ""), append = TRUE, sep = ",", dec = ".", col.names = TRUE, row.names = FALSE)

# For upload to HydroServer
# http://hydroserver.cuahsi.org/Home/ControlledVocabularies
