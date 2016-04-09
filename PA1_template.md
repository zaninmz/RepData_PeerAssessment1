# Assignment #1
MZANIN  
April 9, 2016  
---
Introduction

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.


```r
##read data frame
activity <- read.csv("activity.csv", header = TRUE, sep = ",", fill = TRUE)
```


What is mean total number of steps taken per day?

1.Calculate the total number of steps taken per day


```r
##remove cases with missing values
activity_rm_NA <- na.omit(activity)
##Calcuate total # of steps per day
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
activity_stepsxday <- group_by(activity_rm_NA, date)
activity_stepsxday_sum <- summarize(activity_stepsxday, sum(steps))
head(activity_stepsxday_sum)
```

```
## Source: local data frame [6 x 2]
## 
##         date sum(steps)
##       (fctr)      (int)
## 1 2012-10-02        126
## 2 2012-10-03      11352
## 3 2012-10-04      12116
## 4 2012-10-05      13294
## 5 2012-10-06      15420
## 6 2012-10-07      11015
```
2. Make a histogram of the total number of steps taken each day


```r
hist(activity_stepsxday_sum$`sum(steps)`, breaks = 10, col = "blue", main = "Total # of Steps per Day", xlab = "Days", ylab = "Total Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)
3.Calculate and report the mean and median of the total number of steps taken per day

```r
##calculate mean number of steps taken per day
activity_stepsxday_mean <- summarize(activity_stepsxday, mean(steps))
print.data.frame(activity_stepsxday_mean)
```

```
##          date mean(steps)
## 1  2012-10-02   0.4375000
## 2  2012-10-03  39.4166667
## 3  2012-10-04  42.0694444
## 4  2012-10-05  46.1597222
## 5  2012-10-06  53.5416667
## 6  2012-10-07  38.2465278
## 7  2012-10-09  44.4826389
## 8  2012-10-10  34.3750000
## 9  2012-10-11  35.7777778
## 10 2012-10-12  60.3541667
## 11 2012-10-13  43.1458333
## 12 2012-10-14  52.4236111
## 13 2012-10-15  35.2048611
## 14 2012-10-16  52.3750000
## 15 2012-10-17  46.7083333
## 16 2012-10-18  34.9166667
## 17 2012-10-19  41.0729167
## 18 2012-10-20  36.0937500
## 19 2012-10-21  30.6284722
## 20 2012-10-22  46.7361111
## 21 2012-10-23  30.9652778
## 22 2012-10-24  29.0104167
## 23 2012-10-25   8.6527778
## 24 2012-10-26  23.5347222
## 25 2012-10-27  35.1354167
## 26 2012-10-28  39.7847222
## 27 2012-10-29  17.4236111
## 28 2012-10-30  34.0937500
## 29 2012-10-31  53.5208333
## 30 2012-11-02  36.8055556
## 31 2012-11-03  36.7048611
## 32 2012-11-05  36.2465278
## 33 2012-11-06  28.9375000
## 34 2012-11-07  44.7326389
## 35 2012-11-08  11.1770833
## 36 2012-11-11  43.7777778
## 37 2012-11-12  37.3784722
## 38 2012-11-13  25.4722222
## 39 2012-11-15   0.1423611
## 40 2012-11-16  18.8923611
## 41 2012-11-17  49.7881944
## 42 2012-11-18  52.4652778
## 43 2012-11-19  30.6979167
## 44 2012-11-20  15.5277778
## 45 2012-11-21  44.3993056
## 46 2012-11-22  70.9270833
## 47 2012-11-23  73.5902778
## 48 2012-11-24  50.2708333
## 49 2012-11-25  41.0902778
## 50 2012-11-26  38.7569444
## 51 2012-11-27  47.3819444
## 52 2012-11-28  35.3576389
## 53 2012-11-29  24.4687500
```

```r
##calculate median number of steps taken per day
activity_stepsxday_med <- summarize(activity_stepsxday, median(steps))
print.data.frame(activity_stepsxday_med)
```

```
##          date median(steps)
## 1  2012-10-02             0
## 2  2012-10-03             0
## 3  2012-10-04             0
## 4  2012-10-05             0
## 5  2012-10-06             0
## 6  2012-10-07             0
## 7  2012-10-09             0
## 8  2012-10-10             0
## 9  2012-10-11             0
## 10 2012-10-12             0
## 11 2012-10-13             0
## 12 2012-10-14             0
## 13 2012-10-15             0
## 14 2012-10-16             0
## 15 2012-10-17             0
## 16 2012-10-18             0
## 17 2012-10-19             0
## 18 2012-10-20             0
## 19 2012-10-21             0
## 20 2012-10-22             0
## 21 2012-10-23             0
## 22 2012-10-24             0
## 23 2012-10-25             0
## 24 2012-10-26             0
## 25 2012-10-27             0
## 26 2012-10-28             0
## 27 2012-10-29             0
## 28 2012-10-30             0
## 29 2012-10-31             0
## 30 2012-11-02             0
## 31 2012-11-03             0
## 32 2012-11-05             0
## 33 2012-11-06             0
## 34 2012-11-07             0
## 35 2012-11-08             0
## 36 2012-11-11             0
## 37 2012-11-12             0
## 38 2012-11-13             0
## 39 2012-11-15             0
## 40 2012-11-16             0
## 41 2012-11-17             0
## 42 2012-11-18             0
## 43 2012-11-19             0
## 44 2012-11-20             0
## 45 2012-11-21             0
## 46 2012-11-22             0
## 47 2012-11-23             0
## 48 2012-11-24             0
## 49 2012-11-25             0
## 50 2012-11-26             0
## 51 2012-11-27             0
## 52 2012-11-28             0
## 53 2012-11-29             0
```
What is the average daily activity pattern?
1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
##Calculate average number of steps taken by interval across all days
activity_rm_NA$interval <- as.factor(activity_rm_NA$interval)
activity_stepsxinterval <- group_by(activity_rm_NA, interval)
activity_stepsxinterval_mean <- summarize(activity_stepsxinterval, mean(steps))
##create plot
plot(activity_stepsxinterval_mean$interval, activity_stepsxinterval_mean$`mean(steps)`, type = "l",col = "blue", main = "Average Number of Steps per 5 Minute Inteval", xlab = "5 Minute Interval", ylab = "Mean Number of Steps", lwd = 4)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
##find max number of mean steps across intervals
max(activity_stepsxinterval_mean$`mean(steps)`)
```

```
## [1] 206.1698
```

```r
##find row containing max 
grep(206.1698, activity_stepsxinterval_mean$`mean(steps)`)
```

```
## [1] 104
```

```r
##subset df by row to find interval
activity_stepsxinterval_mean[104,]
```

```
## Source: local data frame [1 x 2]
## 
##   interval mean(steps)
##     (fctr)       (dbl)
## 1      835    206.1698
```
Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
##count number of complete cases in df
activity_na <- complete.cases(activity)
table(activity_na)
```

```
## activity_na
## FALSE  TRUE 
##  2304 15264
```

```r
## There are 2,304 rows with missing values
```

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
##impute missing values with interval means
library(plyr)
```

```
## -------------------------------------------------------------------------
```

```
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
```

```
## -------------------------------------------------------------------------
```

```
## 
## Attaching package: 'plyr'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```r
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
activity_impute_mean <- ddply(activity, ~ interval, transform, steps = impute.mean(steps))
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
##sum step by days
activity_impute_sumstepsxday<- aggregate(activity_impute_mean$steps, by=list(date=activity_impute_mean$date), FUN = sum)
##create histogram
hist(activity_impute_sumstepsxday$x, breaks = 10, col = "blue", main = "Total # of Steps per Day", xlab = "Days", ylab = "Total Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)

```r
##Calculate the mean steps by day
activity_impute_mean_meanstepsxday<- aggregate(activity_impute_mean$steps, by=list(date=activity_impute_mean$date), FUN = mean)
print(activity_impute_mean_meanstepsxday)
```

```
##          date          x
## 1  2012-10-01 37.3825996
## 2  2012-10-02  0.4375000
## 3  2012-10-03 39.4166667
## 4  2012-10-04 42.0694444
## 5  2012-10-05 46.1597222
## 6  2012-10-06 53.5416667
## 7  2012-10-07 38.2465278
## 8  2012-10-08 37.3825996
## 9  2012-10-09 44.4826389
## 10 2012-10-10 34.3750000
## 11 2012-10-11 35.7777778
## 12 2012-10-12 60.3541667
## 13 2012-10-13 43.1458333
## 14 2012-10-14 52.4236111
## 15 2012-10-15 35.2048611
## 16 2012-10-16 52.3750000
## 17 2012-10-17 46.7083333
## 18 2012-10-18 34.9166667
## 19 2012-10-19 41.0729167
## 20 2012-10-20 36.0937500
## 21 2012-10-21 30.6284722
## 22 2012-10-22 46.7361111
## 23 2012-10-23 30.9652778
## 24 2012-10-24 29.0104167
## 25 2012-10-25  8.6527778
## 26 2012-10-26 23.5347222
## 27 2012-10-27 35.1354167
## 28 2012-10-28 39.7847222
## 29 2012-10-29 17.4236111
## 30 2012-10-30 34.0937500
## 31 2012-10-31 53.5208333
## 32 2012-11-01 37.3825996
## 33 2012-11-02 36.8055556
## 34 2012-11-03 36.7048611
## 35 2012-11-04 37.3825996
## 36 2012-11-05 36.2465278
## 37 2012-11-06 28.9375000
## 38 2012-11-07 44.7326389
## 39 2012-11-08 11.1770833
## 40 2012-11-09 37.3825996
## 41 2012-11-10 37.3825996
## 42 2012-11-11 43.7777778
## 43 2012-11-12 37.3784722
## 44 2012-11-13 25.4722222
## 45 2012-11-14 37.3825996
## 46 2012-11-15  0.1423611
## 47 2012-11-16 18.8923611
## 48 2012-11-17 49.7881944
## 49 2012-11-18 52.4652778
## 50 2012-11-19 30.6979167
## 51 2012-11-20 15.5277778
## 52 2012-11-21 44.3993056
## 53 2012-11-22 70.9270833
## 54 2012-11-23 73.5902778
## 55 2012-11-24 50.2708333
## 56 2012-11-25 41.0902778
## 57 2012-11-26 38.7569444
## 58 2012-11-27 47.3819444
## 59 2012-11-28 35.3576389
## 60 2012-11-29 24.4687500
## 61 2012-11-30 37.3825996
```

```r
##Calculate the median steps by day
activity_impute_mean_medstepsxday<- aggregate(activity_impute_mean$steps, by=list(date=activity_impute_mean$date), FUN = median)
print(activity_impute_mean_medstepsxday)
```

```
##          date        x
## 1  2012-10-01 34.11321
## 2  2012-10-02  0.00000
## 3  2012-10-03  0.00000
## 4  2012-10-04  0.00000
## 5  2012-10-05  0.00000
## 6  2012-10-06  0.00000
## 7  2012-10-07  0.00000
## 8  2012-10-08 34.11321
## 9  2012-10-09  0.00000
## 10 2012-10-10  0.00000
## 11 2012-10-11  0.00000
## 12 2012-10-12  0.00000
## 13 2012-10-13  0.00000
## 14 2012-10-14  0.00000
## 15 2012-10-15  0.00000
## 16 2012-10-16  0.00000
## 17 2012-10-17  0.00000
## 18 2012-10-18  0.00000
## 19 2012-10-19  0.00000
## 20 2012-10-20  0.00000
## 21 2012-10-21  0.00000
## 22 2012-10-22  0.00000
## 23 2012-10-23  0.00000
## 24 2012-10-24  0.00000
## 25 2012-10-25  0.00000
## 26 2012-10-26  0.00000
## 27 2012-10-27  0.00000
## 28 2012-10-28  0.00000
## 29 2012-10-29  0.00000
## 30 2012-10-30  0.00000
## 31 2012-10-31  0.00000
## 32 2012-11-01 34.11321
## 33 2012-11-02  0.00000
## 34 2012-11-03  0.00000
## 35 2012-11-04 34.11321
## 36 2012-11-05  0.00000
## 37 2012-11-06  0.00000
## 38 2012-11-07  0.00000
## 39 2012-11-08  0.00000
## 40 2012-11-09 34.11321
## 41 2012-11-10 34.11321
## 42 2012-11-11  0.00000
## 43 2012-11-12  0.00000
## 44 2012-11-13  0.00000
## 45 2012-11-14 34.11321
## 46 2012-11-15  0.00000
## 47 2012-11-16  0.00000
## 48 2012-11-17  0.00000
## 49 2012-11-18  0.00000
## 50 2012-11-19  0.00000
## 51 2012-11-20  0.00000
## 52 2012-11-21  0.00000
## 53 2012-11-22  0.00000
## 54 2012-11-23  0.00000
## 55 2012-11-24  0.00000
## 56 2012-11-25  0.00000
## 57 2012-11-26  0.00000
## 58 2012-11-27  0.00000
## 59 2012-11-28  0.00000
## 60 2012-11-29  0.00000
## 61 2012-11-30 34.11321
```
While imputing interval means for missing values did not change the mean and median for comparable days, imputation by intervals increased the total number of days in the data set.

Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
##convert date column to date class
activity_impute_mean$date <- as.Date(activity_impute_mean$date)
##add two level weekday variable to data set
library(chron)
activity_weekdays <- mutate(activity_impute_mean, weekday = (ifelse(is.weekend(date) == "TRUE", "weekend", "weekday")))
activity_weekdays$weekday <- as.factor(activity_weekdays$weekday)
```
Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
##calculate step means grouped by intervals accross weekend and weekday days
activity_weekday_meanxinterval<- aggregate(activity_weekdays$steps, by=list(interval=activity_weekdays$interval, weekday = activity_weekdays$weekday), FUN = mean)
##create plot using lattice package
library(lattice)
par(mfrow = c(2,1))
xyplot(x ~ interval | factor(weekday), data = activity_weekday_meanxinterval, type = "l", ylab = "Mean Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)





