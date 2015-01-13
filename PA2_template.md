Reproducible Research: Peer Assessment 2
==========================================
Created by Simon Mackinnon on January 13, 2015
[Reproducable Research - Coursera](https://www.coursera.org/course/repdata)

## Economic and Public Health Effects of Storms and Other Severe Weather Events 

### Synopsis


### Settings

```r
options(scipen = 999) 
library(R.utils)
library(lubridate)
library(ggplot2)
```

### Data Processing
The data to be used for this analysis is available for download via the course website at [Storm Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2).    

If the .csv file isn't already extracted, and the zip file doesn't exist, then we should download and then extract it.

```r
zip_filename <- "data/repdata-data-StormData.csv.bz2"
data_filename <- "data/repdata-data-StormData.csv"

if (!file.exists(data_filename))
{
  if (!file.exists(zip_filename))
  {
    url <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    #record the time/location of the downloaded file
    print (paste("Downloading file from:", url ,"at:", format(Sys.Date(), "%B %d, %Y"), format(Sys.time(), "%H:%M:%S")))
    #changed url to be http, as download file has issues with https
    
    download.file(url, 
                  destfile = zip_filename,
                  mode="wb")
  }
  
  bunzip2(zip_filename, overwrite=T, remove=F)
}
```

Once we have the .csv file, we need to read it into R.


```r
# this may take some time!
stormData <- read.csv(data_filename, 
                      header=TRUE, 
                      sep=",",
                      stringsAsFactors=FALSE)

#confirm the number dimensions of the read data
dim(stormData)
```

```
## [1] 902297     37
```

```r
#and display the head rows
head(stormData)
```

```
##   STATE__           BGN_DATE BGN_TIME TIME_ZONE COUNTY COUNTYNAME STATE
## 1       1  4/18/1950 0:00:00     0130       CST     97     MOBILE    AL
## 2       1  4/18/1950 0:00:00     0145       CST      3    BALDWIN    AL
## 3       1  2/20/1951 0:00:00     1600       CST     57    FAYETTE    AL
## 4       1   6/8/1951 0:00:00     0900       CST     89    MADISON    AL
## 5       1 11/15/1951 0:00:00     1500       CST     43    CULLMAN    AL
## 6       1 11/15/1951 0:00:00     2000       CST     77 LAUDERDALE    AL
##    EVTYPE BGN_RANGE BGN_AZI BGN_LOCATI END_DATE END_TIME COUNTY_END
## 1 TORNADO         0                                               0
## 2 TORNADO         0                                               0
## 3 TORNADO         0                                               0
## 4 TORNADO         0                                               0
## 5 TORNADO         0                                               0
## 6 TORNADO         0                                               0
##   COUNTYENDN END_RANGE END_AZI END_LOCATI LENGTH WIDTH F MAG FATALITIES
## 1         NA         0                      14.0   100 3   0          0
## 2         NA         0                       2.0   150 2   0          0
## 3         NA         0                       0.1   123 2   0          0
## 4         NA         0                       0.0   100 2   0          0
## 5         NA         0                       0.0   150 2   0          0
## 6         NA         0                       1.5   177 2   0          0
##   INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP WFO STATEOFFIC ZONENAMES
## 1       15    25.0          K       0                                    
## 2        0     2.5          K       0                                    
## 3        2    25.0          K       0                                    
## 4        2     2.5          K       0                                    
## 5        2     2.5          K       0                                    
## 6        6     2.5          K       0                                    
##   LATITUDE LONGITUDE LATITUDE_E LONGITUDE_ REMARKS REFNUM
## 1     3040      8812       3051       8806              1
## 2     3042      8755          0          0              2
## 3     3340      8742          0          0              3
## 4     3458      8626          0          0              4
## 5     3412      8642          0          0              5
## 6     3450      8748          0          0              6
```
There are **902297 rows** and **37 columns** in the read data.


### Data Selection and Filtering

Looking at the details of how the data was collected at the [Storm Events Database](http://www.ncdc.noaa.gov/stormevents/details.jsp), we can see that only observations from 1996 onwards includes data from *All Event Types (48 from Directive 10-1605)*, (as defined in [NWS Directive 10-1605](http://www.ncdc.noaa.gov/stormevents/pd01016005curr.pdf)).  

We can verify that most of the observations were recorded after this date as well, by observing the frequency of observations per year in the **stormData$BGN_DATE** variable


```r
year <- year(as.Date(stormData$BGN_DATE, format = "%m/%d/%Y"))

#break up the year data into 3 types
yearType <- as.numeric(year)

for (i in seq_along(year))
{  
  if (year[i] <= 1955)
  {
    yearType[i] <- 1
  }
  else if (year[i] <= 1996)
  {
    yearType[i] <- 2
  }
  else if (year[i] <= 2011)
  {
    yearType[i] <-3
  }
  else
  {
    yearType[i] <- 0
  }
}

#bind into a marix, then coerce to a data fram
yearData <- data.frame(cbind(year, yearType))

# hist(yearData$year, 
#      breaks = as.numeric(max(yearData$year))-as.numeric(min(yearData$year)),
#      main = "Histogram of Number of\nObservarions Recorded Per Year (1950 - 2011)",
#      xlab = "Year",
#      ylab = "Number of Observations Recorded")

#create the histogram
m <- ggplot(yearData, aes(x=year)) + 
  geom_histogram(binwidth = 1, aes(fill = factor(yearData$yearType))) +
  scale_fill_discrete(  name="Year Type",                        
                        labels= c("1. Tornado\n", 
                                  "2. Tornado,\nThunderstorm Wind\nand Hail\n", 
                                  "3. All Event\nTypes (48 from\nDirective 10-1605)\n"))
#and output
m
```

<img src="figure/observationFrequencyPerYear-1.png" title="plot of chunk observationFrequencyPerYear" alt="plot of chunk observationFrequencyPerYear" style="display: block; margin: auto;" />

Based on this, we filter the data to only use observations from 1996 onwards.


```r
#copy of the originally read data
stormDataOrig <- stormData
#append the year values for filtering
stormData$year <- year
#subset the data for all values of year greater or equal to 1996
stormData <- stormData[stormData$year >= 1996,]
#display the dimensions of the filtered data
dim(storm)
```

```
## Error in eval(expr, envir, enclos): object 'storm' not found
```
After filtering, there are **653530 rows** and **38 columns** in the filtered data.

### Results

#### Economic Effects



#### Public Health Effects




