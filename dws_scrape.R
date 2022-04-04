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
# The format of this file is as follows:
# POS.  1-8   = Date of measurement CCYYMMDD
# POS. 10-15  = Time of measurement HHMMSS
# POS. 27-35  = Corrected level in m
# POS. 37-40  = Quality code
# POS. 52-60  = Corrected flow in cubic metres/sec
# POS. 62-65  = Quality code
# Waiting on quality code key (2022 Apr 04)
base <- "https://www.dws.gov.za/Hydrology/Verified/HyData.aspx?Station="
station <- "A7H004"
variable <- "100.00"
stem1 <- "&DataType=Point&StartDT="
start <- "1955-06-27"
stem2 <- "&EndDT="
end <- "1992-07-15"
tail <- "&SiteType=RIV"
terminate <- force_tz(as_datetime(ymd(end)), tzone = "Africa/Johannesburg")


for (i in 1:100) {
  ## Pull from URL with current start date
  full_url = paste0(base,station,variable,stem1,start,stem2,end,tail)
  api_end <- URLencode(full_url)
  data <- getURL(api_end) # not in JSON, imports as unformatted text.  THIS IS IMPORT LINE!
  data <- strsplit(data,"\n") # separate by line break code
  
  ## Find start and end of data
  for (i in 1:length(data[[1]])) {
    line <- strsplit(data[[1]][i]," ")
    if (line[[1]][1]=="DATE") {
      ln1 <- i+1
    }
    if (line[[1]][1]=="</pre></p>\r") {
      ln2 <- i-1
    }
  }
  
  ## Determine last date and time of data
  line <- strsplit(data[[1]][ln2]," ")
  dt <- ymd_hms(paste0(line[[1]][1],"T",line[[1]][2]))
  dt <- force_tz(dt, tzone = "Africa/Johannesburg")
  dt <- dt+(12*60) # adds 12 minutes to the last time to determine the next time for data
  start <- as.character(date(dt))
  
  ## Split and sort data into table
  x <- foreach(i=ln1:ln2, .combine = 'rbind') %dopar% {
    meas <- array(NA, dim = 5)
    line <- strsplit(data[[1]][i]," ")
    dt <- ymd_hms(paste0(line[[1]][1],"T",line[[1]][2]))
    dt <- force_tz(dt, tzone = "Africa/Johannesburg") # date and time, Unix standard (seconds, UTC), rem with_tz()
    meas[1] <- dt
    column <- 2
    for (j in 3:length(line[[1]])) {
      if (is.na(as.numeric(line[[1]][j]))==FALSE) {
        meas[column] <- as.numeric(line[[1]][j])
        column <- column+1
      }
    }
    # Check to determine if data sorted correctly.  Specifically, missing data may be skipped and the quality flag may be inserted.
    
    print(meas)
  }
  
  ## Write data so far to table
  x <- data.frame(x)
  x <- x %>%
    mutate(dts=as.character(with_tz(as_datetime(X1), tzone = "Africa/Johannesburg")))
  write_csv(x, paste0(station,".csv"), append = TRUE)
  
  ## Check to see if we're at the end
  begin <- force_tz(as_datetime(ymd_hms(start)), tzone = "Africa/Johannesburg")
  if (begin >= terminate) {break}
}


