---
title: "Reproducible Research 1"
output: html_document
---
For this assignment, I need to read in a CSV from the Fitbit activity data.

##Loading and preprocessing the data

First I need certain libraries:


```r
library("plyr")
library("ggplot2")
library("lattice")
```


```r
setwd("~/Documents/datasciencecoursera")
activity<-read.csv("activity.csv")
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```
##What is mean total number of steps taken per day?

To calculate the total number of steps per day, I need to combine all the values for 5 minute intervals (0-2355), and then sum each value for steps for each interval, for each day. For this I will use ```ddply```.


```r
activity<-na.omit(activity)
data<-ddply(activity, .(date), summarize, totalsteps = sum(steps, na.rm = TRUE))
```

Now I make a histogram.


```r
hist(data$totalsteps,main="Histogram of Step Frequencies", xlab="Steps", ylab="Frequency of Occurrence",ylim = c(0, 40))
```

![plot of chunk Histogram with NA](figure/Histogram with NA-1.png) 

Finally, I summarise the mean and median below and print it:



```r
mean(data$totalsteps)
```

```
## [1] 10766.19
```

```r
median(data$totalsteps)
```

```
## [1] 10765
```

##What is the average daily activity pattern?

*Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)*


```r
averagesteps <- ddply(activity, c("interval"), summarise,
               averagesteps = mean(steps)
)

plot(averagesteps$interval, averagesteps$averagesteps, type="l", xlab= "5 minute interval", ylab= "Average Steps Taken", col="green" , lwd=2)
```

![plot of chunk Average daily pattern with NA](figure/Average daily pattern with NA-1.png) 

*Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?*


```r
which.max(averagesteps$averagesteps)
```

```
## [1] 104
```

##Imputing missing values

*Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)*


```r
activity<-read.csv("activity.csv")
sum(is.na(activity))
```

```
## [1] 2304
```

Where steps are NA, I split the data frame by interval, and for that interval across all dates, I then take the median steps. We can see that the median and third quartile change compared to the first dataset. 


```r
activity <- ddply(activity,.(interval), transform, 
                       steps=ifelse(is.na(steps), median(steps, na.rm=TRUE), steps))
summary(activity)
```

```
##      steps             date          interval     
##  Min.   :  0   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0   2012-10-03:  288   Median :1177.5  
##  Mean   : 33   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.:  8   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806   2012-10-06:  288   Max.   :2355.0  
##                (Other)   :15840
```

Here I create a new dataset for the total steps per day, now that we have no missing values.


```r
data<-ddply(activity, .(date,interval), summarize, steps = sum(steps))
summary(data)
```

```
##          date          interval          steps    
##  2012-10-01:  288   Min.   :   0.0   Min.   :  0  
##  2012-10-02:  288   1st Qu.: 588.8   1st Qu.:  0  
##  2012-10-03:  288   Median :1177.5   Median :  0  
##  2012-10-04:  288   Mean   :1177.5   Mean   : 33  
##  2012-10-05:  288   3rd Qu.:1766.2   3rd Qu.:  8  
##  2012-10-06:  288   Max.   :2355.0   Max.   :806  
##  (Other)   :15840
```

Now I make a histogram.


```r
hist(data$steps,main="Histogram of Step Frequencies (with NA values filled in)", xlab="Steps", ylab="Frequency of Occurrence",ylim = c(0, 5000),xlim = c(0, 900))
```

![plot of chunk Histogram without NA](figure/Histogram without NA-1.png) 

Now I report the mean and median total number of steps per day.


```r
mean(data$steps)
```

```
## [1] 32.99954
```

```r
median(data$steps)
```

```
## [1] 0
```

As we can see, the effect of replacing NA values with a number (the median of steps across all intervals, split by date) lowers the overall mean and median of total steps per day. This means that before, ignoring NA values actually meant that the data points were higher on aggregate, though less. Now you have more data points but on aggregate the mean total number of steps per day has gone down. Our histogram is also less neat (there are more occurrences of lower step frequencies than higher). This is because you have filled in the lower tail with a value instead of ignoring.

##Are there differences in activity patterns between weekdays and weekends?

*Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.*


```r
data$day <- weekdays(as.Date(data$date))
data$weekend<-ifelse(!weekdays(as.Date(data$date)) %in% c('Saturday','Sunday'),data$weekend<-"Weekday", data$weekend<-"Weekend")
head(data)
```

```
##         date interval steps    day weekend
## 1 2012-10-01        0     0 Monday Weekday
## 2 2012-10-01        5     0 Monday Weekday
## 3 2012-10-01       10     0 Monday Weekday
## 4 2012-10-01       15     0 Monday Weekday
## 5 2012-10-01       20     0 Monday Weekday
## 6 2012-10-01       25     0 Monday Weekday
```

*Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).*


```r
averagesteps <- ddply(data, c("weekend","interval"), summarise,averagesteps = mean(steps))
head(averagesteps)
```

```
##   weekend interval averagesteps
## 1 Weekday        0   2.02222222
## 2 Weekday        5   0.40000000
## 3 Weekday       10   0.15555556
## 4 Weekday       15   0.17777778
## 5 Weekday       20   0.08888889
## 6 Weekday       25   1.31111111
```

```r
par(mfrow=c(2,1)) 
plot <- xyplot(averagesteps ~ interval | weekend, xlab = "interval",data=averagesteps, type = "l",main = "Average Steps by Interval",layout=c(1,2))
print(plot)
```

![plot of chunk Panel Plot of Weekend Steps](figure/Panel Plot of Weekend Steps-1.png) 
