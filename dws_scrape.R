# Download and analyze the river flow data.
# Beitbridge A7H004 start: 1955-06-27 end: 1992-07-15
# Beitbridge A7H008 start: 1992-07-28 end: 2022-01-27

library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(rjson)
library(RCurl)
library(stringr)
library(devtools)
library(doParallel) # loads parallel and foreach
registerDoParallel(detectCores())

## Prepare URL for data scraping
# Example: https://www.dws.gov.za/Hydrology/Verified/HyData.aspx?Station=A7H008100.00&DataType=Point&StartDT=2021-01-01&EndDT=2022-01-27&SiteType=RIV
# Waiting on quality code key (2022 Apr 04)
base <- "https://www.dws.gov.za/Hydrology/Verified/HyData.aspx?Station="
station <- "X3H008"
variable <- "100.00"
stem1 <- "&DataType=Point&StartDT="
start <- "1967-09-01"
stem2 <- "&EndDT="
end <- "2022-04-08"
tail <- "&SiteType=RIV"
terminate <- force_tz(as_datetime(ymd(end)), tzone = "Africa/Johannesburg")


for (i in 1:100) {
     ## Pull from URL with current start date
     full_url = paste0(base,station,variable,stem1,start,stem2,end,tail)
     api_end <- URLencode(full_url)
     data <- getURL(api_end) # not in JSON, imports as unformatted text.  THIS IS IMPORT LINE!
     data <- strsplit(data,"\n") # separate by line break code
     
     ## Check that data were returned
     if (data[[1]][1] != "No data for requested period.\r") { # Unique flag for DWS data.  Appears robust.
          
          ## Find start and end of data
          for (j in 1:length(data[[1]])) {
               line <- strsplit(data[[1]][j]," ")
               if (line[[1]][1]=="DATE") {
                    ln1 <- j+1
               }
               if (line[[1]][1]=="</pre></p>\r") {
                    ln2 <- j-1
               }
          }
          
          ## Determine last date and time of data
          line <- strsplit(data[[1]][ln2]," ")
          dt <- ymd_hms(paste0(line[[1]][1],"T",line[[1]][2]))
          dt <- force_tz(dt, tzone = "Africa/Johannesburg")
          dt <- dt+(12*60) # adds 12 minutes to the last time to determine the next time for data
          next_start <- as.character(date(dt))
          
          ## Split and sort data into table
          x <- foreach(j=ln1:ln2, .combine = 'rbind') %dopar% {
               meas <- array(NA, dim = 5)
               # The format of this file is as follows:
               # POS.  1-8   = Date of measurement CCYYMMDD
               # POS. 10-15  = Time of measurement HHMMSS
               # POS. 27-35  = Corrected level in m
               # POS. 37-40  = Quality code
               # POS. 52-60  = Corrected flow in cubic metres/sec
               # POS. 62-65  = Quality code
               
               # Parse date and time
               line <- strsplit(data[[1]][j]," ")
               
               dt <- ymd_hms(paste0(line[[1]][1],"T",line[[1]][2]))
               dt <- force_tz(dt, tzone = "Africa/Johannesburg") # date and time, Unix standard (seconds, UTC), rem with_tz()
               meas[1] <- dt
               
               column <- 2
               for (k in 3:length(line[[1]])) {
                    if (is.na(as.numeric(line[[1]][k]))==FALSE) {
                         meas[column] <- as.numeric(line[[1]][k])
                         column <- column+1
                    }
               }
               
               # Check values
               if (is.na(meas[5])) { # check to see if last value is missing; all QC columns are full, so this would mean that the discharge column is empty
                    if (is.na(meas[4])) { # check to see if next-to-last value is missing, would indicate that flow column was missing
                         if (is.na(meas[3])==FALSE) {
                              if (meas[3]==round(meas[3])) { # all QC flags are whole numbers
                                   meas[5] <- meas[3] # this would actually be the discharge QC
                                   meas[4] <- NA # with no discharge data
                              }
                         }
                         if (is.na(meas[2])==FALSE) {
                              if (meas[2]==round(meas[2])) { # all QC flags are whole numbers
                                   meas[3] <- meas[2] # this would actually be the level QC
                                   meas[2] <- NA # with no level data
                              }
                         }
                    } else { # meaning, no last column, but there is a value in the next to last -> only discharge missing
                         if (is.na(meas[4])==FALSE) {
                              if (meas[4]==round(meas[4])) {
                                   meas[5] <- meas[4] # this would actually be the discharge QC
                                   meas[4] <- NA # with no discharge data
                              }
                         }
                    }
               }
               print(meas)
          }
          
          ## Write data so far to table
          # Make sure that the data are in the correct order.  Should be five wide.  If only one date is returned, it will form a vertical matrix.
          if (is.na(ncol(x))) {
               reform <- array(NA, dim = c(1,5))
               for (k in 1:5) {
                    reform[1,k] <- x[k]
               }
               x <- reform
               rm(reform)
          }
          z <- data.frame(x)
          y <- z %>%
               mutate(dt=as.character(with_tz(as_datetime(X1), tzone = "Africa/Johannesburg"))) %>% # Problems arise if there is only one data point.
               mutate(unix=X1, level=X2, levelqc=X3, flow=X4, flowqc=X5) %>%
               select(-X1,-X2,-X3,-X4,-X5)
          # Headers: Date, UNIX date, Level (m), Level QC, Flow (m^3/s), Flow QC
          
          write_csv(y, paste0(station,".csv"), append = TRUE)
          
          ## Check to see if we're importing the same day
          if ((is.na(ymd(next_start))==FALSE)&(is.na(ymd(start))==FALSE)) {
               next_start_lub <- force_tz(as_datetime(ymd(next_start)), tzone = "Africa/Johannesburg") # Next start date determined above based on the date of the last line in the downloaded data
               this_start_lub <- force_tz(as_datetime(ymd(start)), tzone = "Africa/Johannesburg") # Existing start date from the downloaded data (this cycle)
               
               ## Check to see if we're importing the same day
               if (next_start_lub==this_start_lub) {
                    next_start_lub <- next_start_lub + (24*3600) # increase start by one day.
               }
               
               ## Check to see if we're at the end
               if (next_start_lub >= terminate) {break}
               
               ## Reset start
               start <- as.character(date(with_tz(next_start_lub, tzone = "Africa/Johannesburg")))
          }
     } else {
          ## If no data were returned: "No data for requested period.\r"
          this_start_lub <- force_tz(as_datetime(ymd(start)), tzone = "Africa/Johannesburg") # Current start date in lubridate type
          next_start_lub <- this_start_lub + (365*24*3600) # increase start by one year, the approximate amount of time in the primary data pull.
          start <- as.character(date(with_tz(next_start_lub, tzone = "Africa/Johannesburg")))
     }
}

