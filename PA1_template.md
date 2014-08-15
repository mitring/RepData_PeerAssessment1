# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data


```r
stepsData <- read.csv(unz('activity.zip', 'activity.csv'), 
                      colClasses = c("integer", "Date", "integer"))
```

## What is mean total number of steps taken per day?

*1. Make a histogram of the total number of steps taken each day.*


```r
stepsDaily <- data.frame(
  date = unique(stepsData$date), 
  steps = as.numeric(
    tapply(stepsData$steps, stepsData$date, sum, na.rm = TRUE)
  )
)

with (
  stepsDaily, 
  plot(date, steps, 
       main = "Total number of steps taken each day",
       xlab = "Day",
       ylab = "Steps",
       type = 'h'
  )
)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

*2. Calculate and report the mean and median total number of steps taken per day.*


```r
mean(stepsDaily$steps)
```

```
## [1] 9354
```

```r
median(stepsDaily$steps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

*1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).*


```r
stepsInterval <- data.frame(
  interval = unique(stepsData$interval), 
  steps = as.numeric(
    tapply(stepsData$steps, stepsData$interval, mean, na.rm = TRUE)
  )
)

with (
  stepsInterval, 
  plot(interval, steps, 
       main = "Average number of steps taken each 5-minute interval",
       xlab = "Interval",
       ylab = "Steps",
       type = 'l'
  )
)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

*2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?*


```r
stepsInterval[stepsInterval$steps == max(stepsInterval$steps), ]
```

```
##     interval steps
## 104      835 206.2
```

## Imputing missing values

*1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).*


```r
sum(is.na(stepsData$steps))
```

```
## [1] 2304
```

*2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.*

Use the means for 5-minute interval to fill NA values is very useful strategy, because we obtained that means in previous step (see **stepsInterval** data frame).

*3. Create a new dataset that is equal to the original dataset but with the missing data filled in.*


```r
naRows <- as.integer(rownames(stepsData[is.na(stepsData$steps), ]))

stepsDataWithoutNA <- stepsData

for (i in naRows) {
  stepsDataWithoutNA[i, 1] <- 
    stepsInterval[
      stepsInterval$interval == stepsDataWithoutNA[i, 3], 
      2
    ]
}
```

*4. Make a histogram of the total number of steps taken each day.*


```r
stepsDailyWithoutNA <- data.frame(
  date = unique(stepsDataWithoutNA$date), 
  steps = as.numeric(
    tapply(stepsDataWithoutNA$steps, stepsDataWithoutNA$date, sum)
  )
)

with (
  stepsDailyWithoutNA, 
  plot(date, steps, 
       main = "Total number of steps taken each day (without missing values)",
       xlab = "Day",
       ylab = "Steps",
       type = 'h'
  )
)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

*Calculate and report the mean and median total number of steps taken per day.*


```r
mean(stepsDailyWithoutNA$steps)
```

```
## [1] 10766
```

```r
median(stepsDailyWithoutNA$steps)
```

```
## [1] 10766
```

*Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?*

New values for mean and median greater than values obtained in the first part of the assignment. Adding values instead of NA's increase mean and median of the data.

## Are there differences in activity patterns between weekdays and weekends?

*1. Create a new factor variable in the dataset with two levels – "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.*


```r
stepsDataWithoutNA$dayType <- 
  as.factor(
    ifelse(weekdays(stepsDataWithoutNA$date) %in% c("Saturday", "Sunday"), 
           "Weekend", 
           "Weekday"
    )
  )
```

*2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).*


```r
stepsIntervalDayType <- 
  aggregate(steps ~ dayType + interval, stepsDataWithoutNA, mean)

par(mfrow=c(2, 1))

with (
  stepsIntervalDayType[stepsIntervalDayType$dayType == "Weekend", ], 
  plot(interval, steps, 
       main = "Weekend",
       xlab = "Interval",
       ylab = "Steps",
       type = 'l'
  )
)

with (
  stepsIntervalDayType[stepsIntervalDayType$dayType == "Weekday", ], 
  plot(interval, steps, 
       main = "Weekday",
       xlab = "Interval",
       ylab = "Steps",
       type = 'l'
  )
)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 
