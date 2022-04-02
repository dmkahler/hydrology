# Download and analyze the river flow data.
# Beitbridge 1955-1992 A7H004
# Beitbridge A7H008 start: 1992-07-28

library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(rjson)
library(RCurl)
library(stringr)
library(devtools)
# install_github("LimpopoLab/hydrostats")
library(hydrostats)
library(doParallel) # loads parallel and foreach
registerDoParallel(detectCores())

## Prepare URL for data scraping
# Example: https://www.dws.gov.za/Hydrology/Verified/HyData.aspx?Station=A7H008100.00&DataType=Point&StartDT=2021-01-01&EndDT=2022-01-27&SiteType=RIV
base <- "https://www.dws.gov.za/Hydrology/Verified/HyData.aspx?Station="
station <- "A7H008"
variable <- "100.00"
stem1 <- "&DataType=Point&StartDT="
start <- "1992-07-28"
stem2 <- "&EndDT="
end <- "2022-01-27"
tail <- "&SiteType=RIV"

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
    print(meas)
  }
  
  ## Write data so far to table
  x <- data.frame(x)
  x <- x %>%
    mutate(dts=as.character(with_tz(as_datetime(X1), tzone = "Africa/Johannesburg")))
  write_csv(x, paste0(station,".csv"), append = TRUE)
}


