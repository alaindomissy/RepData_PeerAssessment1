# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
# load the data
# setwd('~/Courses/DataScienceSpecialization/5-reproducible-research/peer-assessment-1/RepData_PeerAssessment1')
activity <- read.csv("../activity.csv", header = TRUE, colClasses = c("numeric", 
    "Date", "numeric"), na.strings = "NA")
# convert interval column to a factor
activity$interval <- as.factor(sprintf("%02d%02d", activity$interval%/%100, 
    activity$interval%%100))
# add a datetime column
activity$datetime <- strptime(paste(activity$date, activity$interval), "%F %H%M")
# str(activity)
```



## What is mean total number of steps taken per day?

Histogram of the total number of steps taken each day :

```r
dailytotals <- aggregate(steps ~ date, data = activity, FUN = sum)
hist(dailytotals$steps, breaks = 61, main = "Total Number of Steps Taken Each Day", 
    xlab = "Daily Steps Total")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

The mean total number of steps taken per day is : 

```r
mean(dailytotals$steps)
```

```
## [1] 10766
```

The median total number of steps taken per day is : 

```r
median(dailytotals$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?


```r
# Make a time series plot (i.e. type = 'l') of the 5-minute interval
# (x-axis) and the average number of steps taken, averaged across all days
# (y-axis)
meanintervalsteps <- aggregate(steps ~ interval, data = activity, FUN = mean)
plot(meanintervalsteps, type = "b", main = "Average Daily Activity Pattern", 
    xlab = "5-minute Interval", ylab = "Steps Taken Averaged Over All Days")
lines(meanintervalsteps)
```

![plot of chunk daily pattern](figure/daily_pattern.png) 


The maximum number of steps is taken during this 5-minute interval :

```r
# Which 5-minute interval, on average across all the days in the dataset,
# contains the maximum number of steps?
meanintervalsteps[which.max(meanintervalsteps$steps), ]
```

```
##     interval steps
## 104     0835 206.2
```




## Imputing missing values

There are a number of days/intervals where there are missing values (coded as NA). 
The presence of missing days may introduce bias into some calculations or summaries of the data.

The total number of missing values in the dataset (i.e. the total number of rows with NAs) is :

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```


In order to fill in all of the missing values in the dataset, let's use the following strategy: 
For any row with a missing steps value, let's replace the missing value with the mean value for the corresponding 5-minute interval

```r
# match the rows with their corresponding interval mean
matched <- merge(activity, meanintervalsteps, by = "interval")
# sort by datetime
matched <- matched[order(matched$datetime), ]
# replace na values
activity$steps[is.na(activity$steps)] <- matched$steps.y[is.na(activity$steps)]
```


After replacing NAs, let's plot the histogram of the total number of steps taken each day :

```r
dailytotals = aggregate(steps ~ date, sum, data = activity)
hist(dailytotals$steps, breaks = 61, main = "Total Number of Steps Taken Each Day", 
    xlab = "Daily Steps Total")
```

![plot of chunk new histogram](figure/new_histogram.png) 

After replacing NAs, the mean total number of steps taken per day is : 

```r
mean(dailytotals$steps)
```

```
## [1] 10766
```

After replacing NAs, the median total number of steps taken per day is : 

```r
median(dailytotals$steps)
```

```
## [1] 10766
```

These values do not differ much from the estimates from the first part of the assignment.
Imputing missing data on the estimates of the total daily number of steps results in higher frequencies in the histogram as more values are available, but the mean and the median are little changed. This is a good sign, validating the approach chosen to fill-in the NAs. 



## Are there differences in activity patterns between weekdays and weekends?

Let's use the dataset with the filled-in missing values for this part.

Let's create a new factor variable in the dataset with two levels, “weekday” and “weekend”
indicating whether a given date is a weekday or weekend day:

```r
activity$isweekend <- (weekdays(activity$datetime) %in% c("Saturday", "Sunday"))
```


Let's make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis):

```r
meanintervalstepsweekends = aggregate(steps ~ interval, mean, data = activity[activity$isweekend, 
    ])
meanintervalstepsweekdays = aggregate(steps ~ interval, mean, data = activity[!activity$isweekend, 
    ])
par(mfrow = c(2, 1))
plot(meanintervalstepsweekends, main = "Weekends", ylab = "Number of steps")
lines(meanintervalstepsweekends)
plot(meanintervalstepsweekdays, main = "Weekdays", ylab = "Steps of steps")
lines(meanintervalstepsweekdays)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

The weekend days show activuty levels spread throughout the day, whereas weekdays days have reduced activity level during typical office hours.
Also weekdays days activity starts earlier than weekends days activity.
