---
title: "Reproducible Research: Peer Assessment 1"
author: "Adrian Luna Ortega"
date: "7/Feb/2022"

output: 
  html_document:
    keep_md: true
---
## Set up packages and libraries


```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
install.packages("ggpubr", repos = "http://cran.us.r-project.org")
```

```
## Installing package into 'C:/Users/lunaa/Documents/R/win-library/4.1'
## (as 'lib' is unspecified)
```

```
## package 'ggpubr' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
## 	C:\Users\lunaa\AppData\Local\Temp\Rtmpye46ny\downloaded_packages
```

```r
library(ggpubr)
```

## Loading and preprocessing the data

```r
temporal_File <- paste(getwd(),"activity.zip",sep="/")
unzip(zipfile = temporal_File, exdir = getwd())
activity <- read.csv2("activity.csv",header = TRUE, sep = ",", na.strings = "NA", stringsAsFactors = FALSE)
rm(temporal_File)
activity$date <- ymd(activity$date)
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

Calculate the total number of steps taken per day:

```r
total_number_of_steps_taken_per_day <- activity %>% 
  select(date,steps) %>% 
  group_by(date) %>% 
  summarise(steps = sum(steps, na.rm = TRUE))
```
Make a histogram of the total number of steps taken each day:

```r
hist(total_number_of_steps_taken_per_day$steps, main = "Total number of steps taken per day", xlab = "Steps taken per day", breaks = seq(0,25000, by =1000))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Calculate and report the mean and median of the total number of steps taken per day

The mean is:

```r
mean(total_number_of_steps_taken_per_day$steps)
```

```
## [1] 9354.23
```
The median is:

```r
median(total_number_of_steps_taken_per_day$steps)
```

```
## [1] 10395
```
## What is the average daily activity pattern?

Preparing data:

```r
average_daily_activity_pattern <-  activity %>% 
  select(interval,steps) %>% 
  group_by(interval) %>% 
  summarise(steps = mean(steps, na.rm = TRUE))
```

Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken,
averaged across all days (y-axis)


```r
tsp<- ggplot(average_daily_activity_pattern,aes(x=interval,y=steps)) + geom_line()
tsp
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
average_daily_activity_pattern[which.max(average_daily_activity_pattern$steps),]$interval
```

```
## [1] 835
```

## Imputing missing values

1.- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs):

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

2.- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3.- Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity_imputed <- activity

index_stop <- nrow(activity_imputed)
for(index in 1:index_stop)
{
  if(is.na(activity_imputed[index,]$steps))
  {
    interval <- activity_imputed[index,]$interval
    activity_imputed[index,]$steps <- average_daily_activity_pattern[average_daily_activity_pattern$interval==interval,]$steps
  }
}
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
total_number_of_steps_taken_per_day <- activity_imputed %>% 
  select(date,steps) %>% 
  group_by(date) %>% 
  summarise(steps = sum(steps, na.rm = TRUE))
hist(total_number_of_steps_taken_per_day$steps, main = "Total number of steps taken per day", xlab = "Steps taken per day", breaks = seq(0,25000, by =1000))
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

The mean is:

```r
mean(total_number_of_steps_taken_per_day$steps)
```

```
## [1] 10766.19
```

The median is:

```r
median(total_number_of_steps_taken_per_day$steps)
```

```
## [1] 10766.19
```

Do these values differ from the estimates from the first part of the assignment? Yes

What is the impact of imputing missing data on the estimates of the total daily number of steps? The reduction of Values around zero also mean and media increases.


## Are there differences in activity patterns between weekdays and weekends?

Preparing the data:


```r
activity_imputed$weekday <- sapply(activity_imputed$date, function(x) {
  if(weekdays(x) == "sábado" || weekdays(x) == "domingo") {y<-FALSE} else {y<-TRUE}
})

activity_imputed$weekend <- sapply(activity_imputed$date, function(x) {
  if(weekdays(x) == "sábado" || weekdays(x) == "domingo") {y<-TRUE} else {y<-FALSE}
})
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
average_weekday_activity_pattern <-  activity_imputed %>% 
  filter(weekday==TRUE) %>%
  select(interval,steps) %>% 
  group_by(interval) %>% 
  summarise(steps = mean(steps, na.rm = TRUE))

average_weekend_activity_pattern <-  activity_imputed %>% 
  filter(weekend==TRUE) %>%
  select(interval,steps) %>% 
  group_by(interval) %>% 
  summarise(steps = mean(steps, na.rm = TRUE))

ts_weekday<- ggplot(average_weekday_activity_pattern,aes(x=interval,y=steps)) + geom_line() + labs(title="Average daily steps by weekdays",x="Interval",y="Avg # Steps")
ts_weekend<- ggplot(average_weekend_activity_pattern,aes(x=interval,y=steps)) + geom_line()+ labs(title="Average daily steps by weekend",x="Interval",y="Avg # Steps")

figure <- ggarrange(ts_weekday, ts_weekend, ncol=1,nrow=2)
figure
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->
