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
library(readr)
library(ggplot2)
library(dplyr)
library(rjson)
library(lubridate)
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
setwd("") # Set your working directory - to save data
# setwd("")
site <- "siteName" # name your site - no spaces
today <- Sys.Date()
exportJSON <- toJSON(hydromet)
save(exportJSON, file = paste0(site, "_", today, ".JSON", ""))
rm(exportJSON)







# Check data download
tracker <- array("NA", dim=c(1,6))
tracker[1,1] <- paste(as.character(as_datetime(now(), "UTC")), "UTC") # the current/download time in UTC
tracker[1,2] <- (mrid+1) # record start reference
begin <- hydromet$device$timeseries[[1]]$configuration$values[[1]][[1]] # first date/time in download, in seconds from 1970-01-01
tracker[1,3] <- paste(as.character(as_datetime(as_datetime(begin))), "UTC") # first date/time in download, in date format
num_val <- length(hydromet$device$timeseries[[1]]$configuration$values) # number of recordings
tracker[1,4] <- num_val
endd <- hydromet$device$timeseries[[1]]$configuration$values[[num_val]][[1]] # last date/time in download, in seconds from 1970-01-01
tracker[1,5] <- paste(as.character(as_datetime(as_datetime(endd))), "UTC") # last date/time in download, in date format
tracker[1,6] <- (mrid+num_val)
# headers: NUM,DOWNLOAD_DATE_TIME,START_MRID,BEGIN_DATE_TIME,NUMBER_OF_RECORDS,LAST_MRID  
write.table(tracker, file = paste0(site, "_mrid.csv", ""), append = TRUE, sep = ",", dec = ".", col.names = FALSE)
# Check number of recordings:
print(paste0("Number of recordings: ", num_val))
print(paste0("Number of time steps (inclusive): ", (((endd-begin)/900)+1))) # 900 is # of seconds in a 15 minute interval, the plus one is to include the beginning step
print("Check that these values are the same")
print(paste0("End date and time: ", tracker[1,5])) # end date/time in record
print(paste0("Current date and time: ", tracker[1,1])) # current date/time in record
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
# hydromet$device$timeseries[[1]]$configuration$values[[i]][[5]][[1]]$value    Water Level              (mm)
# hydromet$device$timeseries[[1]]$configuration$values[[i]][[5]][[2]]$value    Water Temperature        (degrees C)
# hydromet$device$timeseries[[1]]$configuration$values[[i]][[5]][[3]]$value    Conductivity             (mS/cm)
# hydromet$device$timeseries[[1]]$configuration$values[[i]][[5 or 6]][[1]]$value    Battery Percent          (%)
# hydromet$device$timeseries[[1]]$configuration$values[[i]][[5 or 6]][[2]]$value    Battery Voltage          (mV)
# hydromet$device$timeseries[[1]]$configuration$values[[i]][[6 or 7]][[1]]$value    Reference Pressure       (kPa)
# hydromet$device$timeseries[[1]]$configuration$values[[i]][[6 or 7]][[2]]$value    Logger Temperature       (degrees C)
# $description   text
# $value         number
# $unit          text
# $error         logical: TRUE or FALSE

utc_offset <- 3600 * 2 # UTC offset for local time

# Preallocation:
DATE <- array(NA, dim = num_val) # date and time in seconds from 00:00 01 Jan 1970, UTC; local time is SAST (UTC+2) or EAT (UTC+3)
DATEqc <- array(0, dim = num_val)
YEAR <- DATE # year
MNTH <- YEAR # month
DAYN <- YEAR # day of month
HOUR <- YEAR # hour
MINU <- YEAR # minute
PRCP <- YEAR # precipitation (mm)
PRCPqc <- array(0, dim = num_val)
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
RIVS <- YEAR
RIVSqc <- PRCPqc
WTMP <- YEAR
WTMPqc <- PRCPqc
COND <- YEAR
CONDqc <- PRCPqc
TRBD <- array(-8888, dim = num_val)
TRBDqc <- PRCPqc
# This contains the relevant parameters for the Limpopo Resilience Lab stations: duq.edu/limpopo

# Check date and time for consistency:
for (i in 2:num_val) {
      # Test that the interval is at least 15 minutes (900 seconds)
      if (hydromet$device$timeseries[[1]]$configuration$values[[i]][[1]][[1]] < (hydromet$device$timeseries[[1]]$configuration$values[[i-1]][[1]][[1]] + 900)) {
            DATEqc[i] <- DATEqc[i] + 900 # error flag for problem with possible repeated values or timing problem
      }
      # Test that the interval is no more than 15 minutes
      if (hydromet$device$timeseries[[1]]$configuration$values[[i]][[1]][[1]] > (hydromet$device$timeseries[[1]]$configuration$values[[i-1]][[1]][[1]] + 900)) {
            DATEqc[i] <- DATEqc[i] + 90 # error flag for problem with possible repeated values or timing problem
      }
}
max(DATEqc)
# which.max(DATEqc)

for (i in 1:num_val) {
      DATE[i] <- hydromet$device$timeseries[[1]]$configuration$values[[i]][[1]][[1]] # DATE is in UTC
      dt <- as_datetime(DATE[i]) + utc_offset # Convert: UTC to SAST, UTC+2 hours, or 7200 seconds in South Africa, set this value at line 115
      YEAR[i] <- year(dt) # LOCAL TIME
      MNTH[i] <- month(dt) # LOCAL TIME
      DAYN[i] <- day(dt) # LOCAL TIME
      HOUR[i] <- hour(dt) # LOCAL TIME
      MINU[i] <- min(dt) # LOCAL TIME
      # SOLAR RADIATION
      if (is.numeric(hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[1]]$value)) {
            SRAD[i] <- hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[1]]$value
            if (SRAD[i] < 0) {
                  SRADqc[i] <- SRADqc[i] + 5 # detects and indicates a value below range
            }
            if (SRAD[i] > 2000) {
                  SRADqc[i] <- SRADqc[i] + 6 # detects and indicates a value above range
            }
            if (i > 3) {
                  if ((!is.na(SRAD[i-1])) & (!is.na(SRAD[i-2]))) { # tests if there are sufficient values and both previous values are not NA, !is.na()
                        if ((SRAD[i] == SRAD[i-1]) & (SRAD[i] == SRAD[i-2])) {
                              SRADqc[i] <- SRADqc[i] + 90 # detects possible stuck value
                        }
                  }
            }
      } else {
            SRADqc[i] <- SRADqc[i] + 900 # Logger error - no value
      }
      if (hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[1]]$error) {
            SRADqc[i] <- SRADqc[i] + 9000 # Logger error
      }
      # PRECIPITATION
      if (is.numeric(hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[2]]$value)) {
            PRCP[i] <- hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[2]]$value
            if (PRCP[i] < 0) {
                  PRCPqc[i] <- PRCPqc[i] + 5 # detects and indicates a value below range
            }
            if (PRCP[i] > 200) {
                  PRCPqc[i] <- PRCPqc[i] + 6 # detects and indicates a value above range
            }
            if (i > 4) {
                  if ((!is.na(PRCP[i-1])) & (!is.na(PRCP[i-2])) & (!is.na(PRCP[i-3])) & (!is.na(PRCP[i-4]))) {
                        if ((PRCP[i] == PRCP[i-1]) & (PRCP[i] == PRCP[i-2]) & (PRCP[i] == PRCP[i-3]) & (PRCP[i] == PRCP[i-4])) {
                              PRCPqc[i] <- PRCPqc[i] + 90 # detects possible stuck value
                        }
                  }
            }
      } else {
            PRCPqc[i] <- PRCPqc[i] + 900 # Logger error - no value
      }
      if (hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[2]]$error) {
            PRCPqc[i] <- PRCPqc[i] + 9000 # Logger error
      }
      # WIND DIRECTION
      if (is.numeric(hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[5]]$value)) {
            WDIR[i] <- hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[5]]$value
            if (WDIR[i] < 0) {
                  WDIRqc[i] <- WDIRqc[i] + 5 # detects and indicates a value below range
            }
            if (WDIR[i] > 360) {
                  WDIRqc[i] <- WDIRqc[i] + 6 # detects and indicates a value above range
            }
            if (i > 3) {
                  if ((!is.na(WDIR[i-1])) & (!is.na(WDIR[i-2]))) {
                        if ((WDIR[i] == WDIR[i-1]) & (WDIR[i] == WDIR[i-2])) {
                              WDIRqc[i] <- WDIRqc[i] + 90 # detects possible stuck value
                        }
                  }
            }
      } else {
            WDIRqc[i] <- WDIRqc[i] + 900 # Logger error - no value
      }
      if (hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[5]]$error) {
            WDIRqc[i] <- WDIRqc[i] + 9000 # Logger error
      }
      # WIND SPEED
      if (is.numeric(hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[6]]$value)) {
            WSPD[i] <- hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[6]]$value
            if (WSPD[i] < 0) {
                  WSPDqc[i] <- WSPDqc[i] + 5 # detects and indicates a value below range
            }
            if (WSPD[i] > 100) {
                  WSPDqc[i] <- WSPDqc[i] + 6 # detects and indicates a value above range
            }
            if (i > 3) {
                  if ((!is.na(WSPD[i-1])) & (!is.na(WSPD[i-2]))) {
                        if ((WSPD[i] == WSPD[i-1]) & (WSPD[i] == WSPD[i-2])) {
                              WSPDqc[i] <- WSPDqc[i] + 90 # detects possible stuck value
                        }
                  }
            }
      } else {
            WSPDqc[i] <- WSPDqc[i] + 900 # Logger error - no value
      }
      if (hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[6]]$error) {
            WSPDqc[i] <- WSPDqc[i] + 9000 # Logger error
      }
      # AIR TEMPERATURE
      if (is.numeric(hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[8]]$value)) {
            TEMP[i] <- hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[8]]$value
            if (TEMP[i] < -15) {
                  TEMPqc[i] <- TEMPqc[i] + 5 # detects and indicates a value below range
            }
            if (TEMP[i] > 55) {
                  TEMPqc[i] <- TEMPqc[i] + 6 # detects and indicates a value above range
            }
            if (i > 3) {
                  if ((!is.na(TEMP[i-1])) & (!is.na(TEMP[i-2]))) {
                        if ((TEMP[i] == TEMP[i-1]) & (TEMP[i] == TEMP[i-2])) {
                              TEMPqc[i] <- TEMPqc[i] + 90 # detects possible stuck value
                        }
                  }
            }
      } else {
            TEMPqc[i] <- TEMPqc[i] + 900 # Logger error - no value
      }
      if (hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[8]]$error) {
            TEMPqc[i] <- TEMPqc[i] + 9000 # Logger error
      }
      # RELATIVE HUMIDITY
      if ((is.numeric(hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[9]]$value)) & (is.numeric(TEMP[i]))) {
            at <- TEMP[i] + 273.15 # convert air temperature to Kelvin
            svp <- (6984.505294+at*(-188.903931+at*(2.133357675+at*(-0.01288580973+at*(0.00004393587233+at*(-0.00000008023923082+at*6.136820929E-11))))))/10 # compute saturation vapor pressure in kPa via the Goff-Gratch equation, in nested form (Lowe, 1977; Brutsaert, 2005)
            vp <- hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[9]]$value # import vapor pressure in kPa
            RHMD[i] <- round(100*(vp/svp), digits = 1) # compute relative humidity in %
            if (vp < 0) {
                  RHMDqc[i] <- RHMDqc[i] + 5 # detects and indicates a value below range
            }
            if (vp > 10) {
                  RHMDqc[i] <- RHMDqc[i] + 6 # detects and indicates a value above range
            }
            if (i > 3) {
                  if ((!is.na(RHMD[i-1])) & (!is.na(RHMD[i-2]))) {
                        if ((RHMD[i] == RHMD[i-1]) & (RHMD[i] == RHMD[i-2])) {
                              RHMDqc[i] <- RHMDqc[i] + 90 # detects possible stuck value
                        }
                  }
            }
      } else {
            RHMDqc[i] <- RHMDqc[i] + 900 # Logger error - no value
      }
      if (hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[9]]$error) {
            RHMDqc[i] <- RHMDqc[i] + 9000 # Logger error
      }
      # AIR PRESSURE
      if (is.numeric(hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[10]]$value)) {
            APRS[i] <- hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[10]]$value
            if (APRS[i] < 65) {
                  APRSqc[i] <- APRSqc[i] + 5 # detects and indicates a value below range
            }
            if (APRS[i] > 110) {
                  APRSqc[i] <- APRSqc[i] + 6 # detects and indicates a value above range
            }
            if (i > 3){
                  if ((!is.na(APRS[i-1])) & (!is.na(APRS[i-2]))) {
                        if ((APRS[i] == APRS[i-1]) & (APRS[i] == APRS[i-2])) {
                              APRSqc[i] <- APRSqc[i] + 90 # detects possible stuck value
                        }
                  }
            }
      } else {
            APRSqc[i] <- APRSqc[i] + 900 # Logger error - no value
      }
      if (hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[10]]$error) {
            APRSqc[i] <- APRSqc[i] + 9000 # Logger error
      }
      # TILT
      if ((is.numeric(hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[11]]$value)) & (is.numeric(hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[12]]$value))) {
            x <- hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[11]]$value
            y <- hydromet$device$timeseries[[1]]$configuration$values[[i]][[4]][[12]]$value
            tilt <- sqrt((x^2)+(y^2))
            if (tilt >= 2) {
                  PRCPqc[i] <- PRCPqc[i] + 90000
                  SRADqc[i] <- SRADqc[i] + 90000
                  WSPDqc[i] <- WSPDqc[i] + 90000
                  WDIRqc[i] <- WDIRqc[i] + 90000
                  print(paste0("Sensors tilted past 2 degrees at index ", i, " at time ", dt)) # error time
            }
      } else {
            PRCPqc[i] <- PRCPqc[i] + 80000
            SRADqc[i] <- SRADqc[i] + 80000
            WSPDqc[i] <- WSPDqc[i] + 80000
            WDIRqc[i] <- WDIRqc[i] + 80000
      }
      # RIVER STAGE
      if (is.numeric(hydromet$device$timeseries[[1]]$configuration$values[[i]][[5]][[1]]$value)) {
            RIVS[i] <- (hydromet$device$timeseries[[1]]$configuration$values[[i]][[5]][[1]]$value-875)/1000 # measured above the wier crest, to coincide with the staff gage at the weir, sensor is located 0.875 m below the weir crest, converted to m from mm
            if (RIVS[i] < -5) {
                  RIVSqc[i] <- RIVSqc[i] + 5 # detects and indicates a value below range
            }
            if (RIVS[i] > 5) {
                  RIVSqc[i] <- RIVSqc[i] + 6 # detects and indicates a value above range
            }
            # if ((i > 5) & (!is.na(APRS[i-1])) & (!is.na(APRS[i-2])) & (!is.na(APRS[i-3])) & (!is.na(APRS[i-4])) & (!is.na(APRS[i-5]))) { # UNSURE IF THIS IS GOING TO HELP OR HURT.
            #       if ((RIVS[i] == RIVS[i-1]) & (RIVS[i] == RIVS[i-2]) & (RIVS[i] == RIVS[i-3]) & (RIVS[i] == RIVS[i-4]) & (RIVS[i] == RIVS[i-5])) {
            #             RIVSqc[i] <- RIVSqc[i] + 90 # detects possible stuck value
            #       }
            # }
      } else {
            RIVSqc[i] <- RIVSqc[i] + 900 # Logger error - no value
      }
      if (hydromet$device$timeseries[[1]]$configuration$values[[i]][[5]][[1]]$error) {
            RIVSqc[i] <- RIVSqc[i] + 9000 # Logger error
      }
      # WATER TEMPERATURE
      if (is.numeric(hydromet$device$timeseries[[1]]$configuration$values[[i]][[5]][[2]]$value)) {
            WTMP[i] <- hydromet$device$timeseries[[1]]$configuration$values[[i]][[5]][[2]]$value
            if (WTMP[i] < 0) {
                  WTMPqc[i] <- WTMPqc[i] + 5 # detects and indicates a value below range
            }
            if (WTMP[i] > 40) {
                  WTMPqc[i] <- WTMPqc[i] + 6 # detects and indicates a value above range
            }
            if (i > 3) {
                  if ((!is.na(WTMP[i-1])) & (!is.na(WTMP[i-2]))) {
                        if ((WTMP[i] == WTMP[i-1]) & (WTMP[i] == WTMP[i-2])) {
                              WTMPqc[i] <- WTMPqc[i] + 90 # detects possible stuck value
                        }
                  }
            }
      } else {
            WTMPqc[i] <- WTMPqc[i] + 900 # Logger error - no value
      }
      if (hydromet$device$timeseries[[1]]$configuration$values[[i]][[5]][[2]]$error) {
            WTMPqc[i] <- WTMPqc[i] + 9000 # Logger error
      }
      # CONDUCTIVITY
      if (is.numeric(hydromet$device$timeseries[[1]]$configuration$values[[i]][[5]][[3]]$value)) {
            COND[i] <- 1000 * hydromet$device$timeseries[[1]]$configuration$values[[i]][[5]][[3]]$value # convert from mS/cm to uS/cm.
            if (COND[i] < -5) {
                  CONDqc[i] <- CONDqc[i] + 5 # detects and indicates a value below range
            }
            if (COND[i] > 5) {
                  CONDqc[i] <- CONDqc[i] + 6 # detects and indicates a value above range
            }
            if (i > 3) {
                  if ((!is.na(COND[i-1])) & (!is.na(COND[i-2]))) {
                        if ((COND[i] == COND[i-1]) & (COND[i] == COND[i-2])) {
                              CONDqc[i] <- CONDqc[i] + 90 # detects possible stuck value
                        }
                  }
            }
      } else {
            CONDqc[i] <- CONDqc[i] + 900 # Logger error - no value
      }
      if (hydromet$device$timeseries[[1]]$configuration$values[[i]][[5]][[3]]$error) {
            CONDqc[i] <- CONDqc[i] + 9000 # Logger error
      }
}

# QC flags (in XXXXqc fields):
# 00000 No concerns
# xxxx5 Value below range
# xxxx6 Value above range
# xxx9x Possible stuck value
# xx9xx Non-numeric value
# x9xxx Error passed from datalogger
# 9xxxx Tilt of sensor is out of QC range (2 degrees)
# 8xxxx Tilt data missing

# Store to data frame and export

#df <- data.frame(DATE, YEAR, MNTH, DAYN, HOUR, MINU, PRCP, PRCPqc, SRAD, SRADqc, TEMP, TEMPqc, RHMD, RHMDqc, APRS, APRSqc, WSPD, WSPDqc, WDIR, WDIRqc)
df <- data.frame(DATE, YEAR, MNTH, DAYN, HOUR, MINU, PRCP, PRCPqc, SRAD, SRADqc, TEMP, TEMPqc, RHMD, RHMDqc, APRS, APRSqc, WSPD, WSPDqc, WDIR, WDIRqc, RIVS, RIVSqc, WTMP, WTMPqc, COND, CONDqc, TRBD, TRBDqc)
write_csv(df, paste0(site, "_", today, ".csv"), na = "NA", append = TRUE, eol = "\n") # UNIX standard end of line, comma-delimited, decimal point used "."
# For upload to HydroServer
# http://hydroserver.cuahsi.org/Home/ControlledVocabularies

