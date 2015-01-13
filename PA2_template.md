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
```

```
## Loading required package: R.oo
## Loading required package: R.methodsS3
## R.methodsS3 v1.6.1 (2014-01-04) successfully loaded. See ?R.methodsS3 for help.
## R.oo v1.18.0 (2014-02-22) successfully loaded. See ?R.oo for help.
## 
## Attaching package: 'R.oo'
## 
## The following objects are masked from 'package:methods':
## 
##     getClasses, getMethods
## 
## The following objects are masked from 'package:base':
## 
##     attach, detach, gc, load, save
## 
## R.utils v1.34.0 (2014-10-07) successfully loaded. See ?R.utils for help.
## 
## Attaching package: 'R.utils'
## 
## The following object is masked from 'package:utils':
## 
##     timestamp
## 
## The following objects are masked from 'package:base':
## 
##     cat, commandArgs, getOption, inherits, isOpen, parse, warnings
```

```r
library(lubridate)
library(ggplot2)
```

```
## Find out what's changed in ggplot2 with
## news(Version == "1.0.0", package = "ggplot2")
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

```
## [1] "Downloading file from: http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2 at: January 13, 2015 15:25:52"
```

```
## Error in download.file(url, destfile = zip_filename, mode = "wb"): cannot open destfile 'data/repdata-data-StormData.csv.bz2', reason 'No such file or directory'
```

Once we have the .csv file, we need to read it into R.


```r
# this may take some time!
stormData <- read.csv(data_filename, 
                      header=TRUE, 
                      sep=",",
                      stringsAsFactors=FALSE)
```

```
## Warning in file(file, "rt"): cannot open file
## 'data/repdata-data-StormData.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
#confirm the number dimensions of the read data
dim(stormData)
```

```
## Error in eval(expr, envir, enclos): object 'stormData' not found
```

```r
#and display the head rows
head(stormData)
```

```
## Error in head(stormData): object 'stormData' not found
```









