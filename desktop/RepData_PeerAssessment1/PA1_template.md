# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
setwd("C:/Users/cy/desktop/reproducible research/repdata-data-activity")
data <- read.csv("activity.csv")
```
Then we take a brief look at the data.

```r
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## What is mean total number of steps taken per day?
* Make a histogram of the total number of steps taken each day

```r
Total <- tapply(data$steps, data$date, sum)
hist(Total)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

* Calculate and report the mean and median total number of steps taken per day

```r
library(stats)
Mean <- mean(Total, na.rm = TRUE); Mean
```

```
## [1] 10766
```

```r
Median <- median(Total, na.rm = TRUE); Median
```

```
## [1] 10765
```
The mean total number of steps is 1.0766 &times; 10<sup>4</sup>, and the median is 10765.

## What is the average daily activity pattern?
* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
Interval <- unique(data$interval)
AveInt <- tapply(data$steps, data$interval, mean, na.rm=TRUE)
plot(x = Interval, y = AveInt, type = "l", xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
Max <- max(AveInt)
Max.Int <- Interval[AveInt == Max]
```
Therefore the 835 interval contains the maximum number of steps. 

## Imputing missing values
* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
logic <- is.na(data$steps)
sum(logic)
```

```
## [1] 2304
```
* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. (I use the mean for that 5-minute interval to fill in the miising values)
* Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
rep <- rep(AveInt, each = nlevels(data$date))
steps <- rep(0, nrow(data))
steps[logic] <- rep[logic]
steps[!logic] <- data$steps[!logic]
NewData <- data.frame(steps=steps, interval=data$interval, date=data$date)
```
* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
NewTotal <- tapply(NewData$steps, NewData$date, sum)
hist(NewTotal)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

```r
library(stats)
NewMean <- mean(NewTotal)
NewMedian <- median(NewTotal);
NewMean
```

```
## [1] 10890
```

```r
NewMedian
```

```
## [1] 11015
```
These values differ from the estimates from the first part. The impact of imputing missing data is that it increases the total steps. 

## Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
time<- strptime(data$date, "%Y-%m-%d")
week <- weekdays(time)
logic <- (week == "ÐÇÆÚÁù"|week == "ÐÇÆÚÈÕ")
f <- factor(logic, labels = c("weekday","weekend"))
DF <- data.frame(data, week = f)
```
* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data

```r
library(lattice)
weekday <- subset(DF, week = "weekday")
weekend <- subset(DF, week = "weekend")
AveWeekday <- tapply(weekday$steps, weekday$interval, mean, na.rm = TRUE)
AveWeekend <- tapply(weekend$steps, weekend$interval, mean, na.rm = TRUE)
df <- data.frame(steps = c(AveWeekday, AveWeekend), interval = rep(Interval,2), Week= factor(rep(c(0,1), each = length(Interval)), labels=c("Weekdays", "Weekend")))
xyplot(steps ~ interval|Week, data=df, layout = c(1,2), type = "l")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 
