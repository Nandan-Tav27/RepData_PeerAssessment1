---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
## Here we will unzip and load the data into a variable activity

```r
unzip("./activity.zip")
activity <- read.csv("activity.csv")
```

##Creating a hitsogram of total steps taken each day

```r
stepspd <- aggregate(steps ~ date, activity, sum, na.rm=TRUE)
hist(stepspd$steps, xlab = "steps per day", main ="STEPS PER DAY")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


## What is mean total number of steps taken per day?
##Calculating mean

```r
meanStepspd <- mean(stepspd$steps)
meanStepspd
```

```
## [1] 10766.19
```

##Calculating median

```r
medianspd <- median(stepspd$steps)
medianspd
```

```
## [1] 10765
```

## What is the average daily activity pattern?
##Creating time series plot

```r
png("timeseriesplot.png")
stepspi<-aggregate(steps~interval, data=activity, mean, na.rm=TRUE)
plot(steps~interval, data=stepspi, type="l")
dev.off()
```

```
## RStudioGD 
##         2
```

###5 min interval with the max number of steps

```r
stepspi[which.max(stepspi[,2]),1]
```

```
## [1] 835
```

## Imputing missing values
##Calculating total number of missing values

```r
Total_missing_values <- sum(is.na(activity[,1]))
Total_missing_values 
```

```
## [1] 2304
```
##Filling in missing values(by filling missing values by mean steps per interval)

```r
missingvalues<-is.na(activity[,1])
mv<-mean(stepspi$steps)
```

##Creating a new dataset with missing values filled in

```r
activity_1<-activity
activity_1[missingvalues,1]<-mv
```

##histogram of steps per day after missing values are added

```r
png("stepsperday_missingvaluesimputed.png")
stepspd_1<-aggregate(steps~date, activity_1, sum)
hist(stepspd_1$steps, xlab="Steps per day", main="STEPS PER DAY")
dev.off()
```

```
## RStudioGD 
##         2
```
##Calculating mean and median after adding missing values

```r
total_Stepspd<-aggregate(steps~date, activity_1, sum)
newmean<-mean(total_Stepspd$steps)
newmean
```

```
## [1] 10766.19
```

```r
newmedian<-median(total_Stepspd$steps)
newmedian
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

```r
activity_1$date<-as.Date(activity_1$date)
library(dplyr)
activity_2<-activity_1%>%
        mutate(dayType= ifelse(weekdays(activity_1$date)=="Saturday" | weekdays(activity_1$date)=="Sunday", "Weekend", "Weekday"))
averageStepByDayTypeAndInterval<-activity_2 %>%
  group_by(dayType, interval) %>%
  summarize(averageStepByDay=sum(steps))
```

```
## `summarise()` has grouped output by 'dayType'. You can override using the `.groups` argument.
```

```r
png("weekday_vs_weekend.png")
library(lattice)
with(averageStepByDayTypeAndInterval, 
      xyplot(averageStepByDay ~ interval | dayType, 
      type = "l",      
      main = "Total Steps",
      xlab = "Daily Intervals",
      ylab = "Average Steps"))
dev.off()
```

```
## RStudioGD 
##         2
```
