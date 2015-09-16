# Peer Assessment 1
Clive Petry  
September 15, 2015  
#Analysis of Personal Movement Using Activity Monitoring Devices

##Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This project makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day. The goal of this project is to write a report that answers the questions detailed below.
<br /><br /><br />

###Loading the data (from the working directory) and processing it

```r
echo = TRUE
activity <- read.csv("activity.csv", sep=",", header=TRUE)
```

Looking at the initial entries of the data

```r
echo=TRUE
head(activity)
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

converting date column from factor to Date class

```r
echo = TRUE
activity$date<-as.Date(activity$date)
```
<br /><br />

###What is the mean total number of steps taken per day?

To calculate the mean number of steps per day we must first calculate the total number of steps for each day (and then show the first of them)

```r
echo = TRUE
stepsperday <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)
head(stepsperday)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420
```

Then present the number of steps per day in histogram form

```r
echo = TRUE
hist(stepsperday, breaks=10, xlab = "number of steps per day", main = "Number of steps taken per day", col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

Calculate the mean number of steps per day

```r
echo = TRUE
mean <- mean(stepsperday)
mean <- as.integer(mean)
```

The mean number of steps per day is 9354

<br />

Calculate the median number of steps per day

```r
echo = TRUE
median <- median(stepsperday)
median <- as.integer(median)
```

The median number of steps per day is 10395

<br /><br />


###What is the average daily activity pattern?

To establish the 5 minute intervals, and then calculate the mean number of steps across the 5 minute intervals on the different days (showing the first of them)

```r
echo = TRUE
intervals <- tapply(activity$interval, activity$interval, mean, na.rm=TRUE)
stepsperinterval <- tapply(activity$steps, activity$interval, mean, na.rm=TRUE)
head(stepsperinterval)
```

```
##         0         5        10        15        20        25 
## 1.7169811 0.3396226 0.1320755 0.1509434 0.0754717 2.0943396
```

Then plot the data

```r
echo = TRUE
plot(intervals, stepsperinterval, type="l",
     xlab = "5 min. Interval", ylab = "Average Number of Steps Per 5 min. Interval", main = "Average Daily Activity Pattern",  col ="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

To find which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps and what that maximum is

```r
echo = TRUE
print(stepsperinterval[which.max(stepsperinterval)])
```

```
##      835 
## 206.1698
```
<br /><br /><br />


###Imputing Missing Values
Firstly we must see how many missing values there are, and in which column they are:

```r
echo = TRUE
table(is.na(activity) == TRUE)
```

```
## 
## FALSE  TRUE 
## 50400  2304
```

```r
summary(activity)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```
It is therefore only the "steps" column that contains missing data
<br />

For those missing "steps" values we will fill in the mean number of steps for that interval in a new dataset (called nonaactivity)

```r
echo = TRUE
nonaactivity <- activity
for (i in 1:nrow(activity)){
    if(is.na(activity$steps[i])){
        nonaactivity$steps[i]<- stepsperinterval[[as.character(activity[i, "interval"])]]
    }
}
```
<br />
The new dataset without NAs is then plotted as a histogram 

```r
echo = TRUE
nonastepsperday <- tapply(nonaactivity$steps, nonaactivity$date, sum, na.rm=TRUE)
hist(nonastepsperday, breaks=10, xlab = "number of steps per day", main = "Number of steps taken per day with imputed missing values", col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 

Calculate the mean number of steps per day with missing values imputed

```r
echo = TRUE
nonamean <- mean(nonastepsperday)
nonamean <- as.integer(nonamean)
```

The mean number of steps per day (with missing values imputed) is 10766
<br /><br /><br />
Calculate the median number of steps per day with missing values imputed

```r
echo = TRUE
nonamedian <- median(nonastepsperday)
nonamedian <- as.integer(nonamedian)
```

The median number of steps per day (with missing values imputed) is 10766  
<br /><br />
**Comparing raw data mean and median steps per day with those where the missing values are imputed**

Mean (no imputed values) 9354 , Mean (with imputed values) 10766

*...the difference between the two is 1412*  
<br />
Median (no imputed values) 10395 , Median (with imputed values) 10766

*...the difference between the two is 371*
<br /><br />
*The impact of imputing the missing data is to increase the mean and median number of steps per day*

<br /><br /><br />

#Are there differences in activity patterns between weekdays and weekends?
<br />

Make a new factor variable in the dataset with imputed missing values, the two levels of which are "weekday" (i.e. Monday-Friday incl.) and "weekend" (i.e. Saturday or Sunday)
<br />


```r
echo = TRUE
nonaactivity$day <- ifelse(as.POSIXlt(as.Date(nonaactivity$date))$wday%%6 == 
                                    0, "weekend", "weekday")
nonaactivity$day <- factor(nonaactivity$day, levels = c("weekday", "weekend"))
```
<br />

A panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

*To do this involves firstly separating the data into two subsets according to whether the measurements relate to weekdays or weekends, then working out the mean steps per interval in each of these subsets and plotting these separately.*
<br /><br />


```r
echo = TRUE
library(lattice)
weekdaynonaactivity <- subset(nonaactivity, nonaactivity$day == "weekday")
weekendnonaactivity <- subset(nonaactivity, nonaactivity$day == "weekend")
weekdaynonaactivitymean <- tapply(weekdaynonaactivity$steps, weekdaynonaactivity$interval, mean)
weekendnonaactivitymean <- tapply(weekendnonaactivity$steps, weekendnonaactivity$interval, mean)
weekdayplot <- data.frame(interval = unique(weekdaynonaactivity$interval), avg = as.numeric(weekdaynonaactivitymean), day = rep("weekday", length(weekdaynonaactivitymean)))
weekendplot <- data.frame(interval = unique(weekendnonaactivity$interval), avg = as.numeric(weekendnonaactivitymean), day = rep("weekend", length(weekendnonaactivitymean)))
finalplot <- rbind(weekdayplot, weekendplot)
xyplot(avg ~ interval | day, data = finalplot, type = "l", layout = c(1, 2), 
       ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png) 

*The difference between the weekday and weekend steps per interval seems to be that at the weekend there is a smaller peak but wider spread of steps.*
