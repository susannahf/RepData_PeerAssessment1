# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

The data is in a zipped csv file. We need to unzip and read in the file.
The "date" variable is in character format, and would be more useful if it was converted to a date format.  We add this as an additional column.


```r
rawdata <- read.csv(unzip("activity.zip"))
rawdata$posixDate <- strptime(rawdata$date,"%Y-%m-%d")
```

## What is mean total number of steps taken per day?

### Calculating the total steps per day


```r
daysum <- tapply(rawdata$steps, rawdata$date, sum, na.rm=T)
```

The resulting sums for each day are 0, 126, 11352, 12116, 13294, 15420, 11015, 0, 12811, 9900, 10304, 17382, 12426, 15098, 10139, 15084, 13452, 10056, 11829, 10395, 8821, 13460, 8918, 8355, 2492, 6778, 10119, 11458, 5018, 9819, 15414, 0, 10600, 10571, 0, 10439, 8334, 12883, 3219, 0, 0, 12608, 10765, 7336, 0, 41, 5441, 14339, 15110, 8841, 4472, 12787, 20427, 21194, 14478, 11834, 11162, 13646, 10183, 7047, 0

### Histogram


```r
hist(daysum, xlab="Total number of steps taken per day",main="Histogram of daily step totals")
```

![](PA1_template_files/figure-html/hist-1.png) 

### Mean and median of total daily steps


```r
meansteps <- mean(daysum)
medsteps <- median(daysum)
```

The mean number of steps per day is 9354.2295082 steps, and the median is 10395 steps.

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
