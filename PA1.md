# Reproducible Research: Peer Assessment 1


## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.


## Packages used in the analysis

```r
library(ggplot2)
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

## Loading and preprocessing the data

**1. Load data**

Assuming that the reader has set their working directory using the `setwd` function to the folder of their choice, listed below are the steps to unzip and read the data file, which can be downloaded from [here](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip).

```
unzip("activity.zip")
activity <- read.csv("activity.csv")
```

**2.Process data**

Looking at the data using `head`, we see there are NAs in the data.


```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

Since there are NAs in the data, we will omit the NAs for future analyses.

```
activity.na <- na.omit(activity)
```


## What is mean total number of steps taken per day?

**1. Calculate the total number of steps per day**

```
total.steps <- tapply(activity.na$steps, activity.na$date, sum)
```

**2. Plot the total number of steps per day into a histogram**

```
hist(total.steps, col = "green", main = "", 
    xlab = "Total Number of Steps per Day", breaks = 15)
```

![https://github.com/DayeW/RepData_PeerAssessment1/blob/master/figures/Total_steps_per_day.png](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

### Calculate the mean and median of total steps per day

**1. Mean**

```
mean(total.steps)
```


```
## [1] 10766.19
```

We get an output of **10766.19**.

**2. Median**

```
median(total.steps)
```


```
## [1] 10765
```

We get an output of **10765**.


## What is the average daily activity pattern?

**1. Make a time series plot of the 5 minutes interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**

Here, we take the average number of steps across all days.

```
activity.mean <- aggregate(x = list(steps = activity.na$steps),
                by = list(interval = activity.na$interval), 
                FUN = mean)
```

**Next, we plot the interval (x-axis) and the averaged number of steps across all days (y-axis).**

```
ggplot(activity.mean, aes(x = interval, y = steps)) +
      geom_line(color = "green", size = 0.5) + 
      theme_bw() +
      xlab("5 minute interval") +
      ylab("Average Number of Steps Across all Days")
```

![https://github.com/DayeW/RepData_PeerAssessment1/blob/master/figures/Ave_Num_Steps_by_Interval.png](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

**2. Maximum number of steps** 

Now, we find the 5 minute-interval value, on average for across all days in the dataset, for the maximum number of steps.

```
maximuminterval <- activity.mean[which.max(activity.mean$steps),]
maximuminterval
```


```
##     interval    steps
## 104      835 206.1698
```

We get an output of maximum interval = **835** at the **206th** step.


## Imputing missing values

Note that there are a number of days that were coded `NA`. The presence of missing days may introduce bias into some calculations or summaries of the data. Due to this bias, we processed the NAs out of the data earlier under `activity.na`. We will revert back to the original data file with the NAs, labelled `activity`. The assignment asks us to complete 4 tasks:

**1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)**

```
activitywithNA <- sum(is.na(activity$steps))
activitywithNA
```


```
## [1] 2304
```

We get an output of **2304**.

**2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.**

We will use the mean value for the 5-minute intervals to fill in all of the missing values in the dataset.

**3. Create a new dataset that is equal to the original dataset but with the missing data filled in.**

```
##Replaced each missing value with the mean of the corresponding interval value (see stack overflow "Replacing Missing Values in R with Column Mean")

mean.fill.interval <- function(steps, interval) {
  filled.interval <- NA
  if (!is.na(steps))
    filled.interval <- c(steps)
  else
    filled.interval <- (activity.mean[activity.mean$interval == interval, "steps"])
  return(filled.interval)
}
activity.fill <- activity
activity.fill$steps <- mapply(mean.fill.interval, activity.fill$steps, 
                              activity.fill$interval)
activity.steps <- tapply(activity.fill$steps, activity.fill$date, FUN = sum)
```

Let's take a look at the data with the filled missing values.


```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

**4. Make a histogram of the total number of steps taken each day.**

```
hist(activity.steps, col = "green", main = "", 
    xlab = "Total steps per day", breaks = 15)
```

![https://github.com/DayeW/RepData_PeerAssessment1/blob/master/figures/Total_steps_per_day_with_filledNAs.png](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

**Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?**

**1. Mean:**

```
mean(activity.steps)
```


```
## [1] 10766.19
```
We get an output of **10766.19**.

**2. Median:**

```
median(activity.steps)
```


```
## [1] 10766.19
```

We get an output of **10766.19**.

## Are there differences in activity patterns between weekdays and weekends?

For this part, the assignment asks us to use the dataset with the filled-in missing values. 

**1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.**

```
activity.day <- function(date) {
  if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {"weekend"}
  else {"weekday"}
} 
activity.fill$day <- as.factor(sapply(activity.fill$date, activity.day))
```

Let's take a look at the data with the newly inputted weekdays.


```
##       steps       date interval     day
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```


**2. Make a panel plot containing a time series plot (i.e. type = "1") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).**

```
activity.final <- aggregate(steps ~ interval + day, activity.fill, mean)
ggplot(activity.final, aes(interval, steps)) + 
  geom_line(color = "green") + theme_bw() +
  facet_grid(day ~ .) + xlab("5 minute interval") + ylab("Number of steps")
```

![https://github.com/DayeW/RepData_PeerAssessment1/blob/master/figures/Ave_Num_Steps_by_Weekdays.png](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

From what the time plot shows us, there appears to be a difference in activity patterns between weekdays and weekends. 



