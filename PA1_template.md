# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

An initial step to load in any packages we need for the data analysis  
- ggplot2 for graphs  


```r
library(ggplot2)
```

The first step of the data annlysis is to read in the data from the csv. NOTE: this assumes that the file is located in the current working directory.  
  
There are then a number of processing steps undertaken on the data  
- Firstly, convert the interval values from an integer (5, 15, 100, etc) into time values (0005, 0015, 0100, etc).   
- We do this by adding 4-length of interval variable number of zeros to the start of the value.  
- Then we convert the time into the correct format HH:MM.  
- Finally we paste the date and time together into one variable - datatime.  


```r
dataIn <- read.csv("activity.csv")

dataIn$time <- paste0(mapply(function(x,y) paste0(rep(x,y), collapse=""), 0, 4-nchar(dataIn$interval)), dataIn$interval)

dataIn$time <- format(strptime(dataIn$time, format="%H%M"), format="%H:%M")

dataIn$datetime <- paste(dataIn$date, dataIn$time, sep=" ")
```

We'll remove the "NA" values from the data currently. Later in the analysis we will impute values for these entries.  


```r
stepError <- is.na(dataIn$steps)
filteredData <- dataIn[!stepError,]
```

## What is mean total number of steps taken per day?

Firstly, lets look at the total number of steps taken in each day. We can use tapply to sum the number steps walked each day. 


```r
stepsPerDay <- tapply(filteredData$steps, filteredData$date, sum, simplify = TRUE)
stepsPerDay
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##         NA        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015         NA      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414         NA      10600      10571         NA      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219         NA         NA      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336         NA         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##         NA
```

Looking at the stepsPerDay variable it is clear that there are a number of NA values in the sums - given that the "NA" values have been removed from the intial data it's not clear exactly where these are coming from.

We can the look at the distribution of the total number of steps.   

```r
qplot(stepsPerDay, bins=20)
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

The most frequent number of steps seems to be around 10,000 steps a day. Let's look at a quick summary of the data.   


```r
summary(stepsPerDay)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10760   10770   13290   21190       8
```

So, the average number of steps - both the mean and median - is just over 10,700 steps a day. The data shows quite a range of values, between the maximun of 21,000 and the minimum of 41. This lower values seems suspiciously low, and affected by missing data.   

We can also see that there are eight dates that contain NA values.

## What is the average daily activity pattern?

Secondly, let's look at the average pattern of steps over the course of a day.  


```r
stepsPerTime <- aggregate(steps ~ time, filteredData, mean)

stepsPerTime$time<- as.factor(stepsPerTime$time)

with(stepsPerTime, plot(time, steps, type="n", xlab="Time", ylab="Number of Steps", main="Steps by Time Interval"))
with(stepsPerTime, lines(time, steps, type="l"))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

The plot shows the mean number of steps for each 5-minute interval throughout the day. It can be seen that the largest mean number of steps are around 08:30 - 09:00.  There are also peaks around midday and the end of the working day. Unsurprisingly there is very little activity before 06:00 and after 23:00.

The time of day with the maximum mean number of steps is shown below

```r
stepsPerTime[stepsPerTime$steps==max(stepsPerTime$steps),]
```

```
##      time    steps
## 104 08:35 206.1698
```

## Imputing missing values

Now we will impute values for the "NA" values that were present in the initial data.   
Fistly, lets look at how many "NA" values there are in the data.


```r
totalErrs <- sum(is.na(dataIn$steps))
propErrs <-  round(100*sum(is.na(dataIn$steps))/nrow(dataIn),2)
```

In total there are 2304 "NA" values in the data. This represents around 13.11% of the entries. 
  
To impute the missing values we will use the mean values for each time interval that we calculated in the previous section of the report. If the steps value is "NA" then we take the steps value for the appropriate time interval in the stepsPerTime dataset.  


```r
dataImpute <- dataIn

dataImpute$steps <- ifelse(is.na(dataImpute$steps)==TRUE, stepsPerTime$steps[stepsPerTime$time %in% dataImpute$time], dataImpute$steps)
```

Let's check the top five rows before and after the impution to check it has worked.  

```r
head(dataIn)
```

```
##   steps       date interval  time         datetime
## 1    NA 2012-10-01        0 00:00 2012-10-01 00:00
## 2    NA 2012-10-01        5 00:05 2012-10-01 00:05
## 3    NA 2012-10-01       10 00:10 2012-10-01 00:10
## 4    NA 2012-10-01       15 00:15 2012-10-01 00:15
## 5    NA 2012-10-01       20 00:20 2012-10-01 00:20
## 6    NA 2012-10-01       25 00:25 2012-10-01 00:25
```

```r
head(dataImpute)
```

```
##       steps       date interval  time         datetime
## 1 1.7169811 2012-10-01        0 00:00 2012-10-01 00:00
## 2 0.3396226 2012-10-01        5 00:05 2012-10-01 00:05
## 3 0.1320755 2012-10-01       10 00:10 2012-10-01 00:10
## 4 0.1509434 2012-10-01       15 00:15 2012-10-01 00:15
## 5 0.0754717 2012-10-01       20 00:20 2012-10-01 00:20
## 6 2.0943396 2012-10-01       25 00:25 2012-10-01 00:25
```

We'll also check the number of "NA" values in the data - which is now zero.


```r
sum(is.na(dataImpute$steps))
```

```
## [1] 0
```

We can now compare the imputed dataset with the initial dataset to see what effect the impution process has had on the results.


```r
stepsPerDayImpute <- tapply(dataImpute$steps, dataImpute$date, sum, simplify = TRUE)
qplot(stepsPerDayImpute, bins=20)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

It can be seen that the distribution of steps has shifted to the right, so the peak is now around 11,000 steps, compared with 10,000 previously.

We can look at the impact of the impution on the mean and median also.   


```r
summary(stepsPerDayImpute)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```

```r
summary(stepsPerDay)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10760   10770   13290   21190       8
```

Despite the shift in the distribution shown in the histogram, it can be seen that the impution process only has a small effect on the median - increasing it from 10,760 to 10,770. The impution process has no effect on the mean as we're adding in extra data with a total number of steps per day equal to the mean value.   

## Are there differences in activity patterns between weekdays and weekends?

Finally, lets look to see if there is a difference in the number of steps between weekdays and weekends.  

We create a factor variable with two values - "weekday" and "weekend" - by using the weekdays() funtion to get the name of the day of the week.  


```r
dataImpute$datetime <- as.Date(dataImpute$datetime)
dataImpute$dayName <- weekdays(dataImpute$datetime)

dataImpute$weekType <- 0
dataImpute$weekType[dataImpute$dayName %in% c("Saturday", "Sunday")] <- 1

dataImpute$weekFactor <- factor(dataImpute$weekType, labels=c("weekday", "weekend"))

head(dataImpute)
```

```
##       steps       date interval  time   datetime dayName weekType
## 1 1.7169811 2012-10-01        0 00:00 2012-10-01  Monday        0
## 2 0.3396226 2012-10-01        5 00:05 2012-10-01  Monday        0
## 3 0.1320755 2012-10-01       10 00:10 2012-10-01  Monday        0
## 4 0.1509434 2012-10-01       15 00:15 2012-10-01  Monday        0
## 5 0.0754717 2012-10-01       20 00:20 2012-10-01  Monday        0
## 6 2.0943396 2012-10-01       25 00:25 2012-10-01  Monday        0
##   weekFactor
## 1    weekday
## 2    weekday
## 3    weekday
## 4    weekday
## 5    weekday
## 6    weekday
```

Now we want to aggregate the number of steps by time interval and the type of day (weekend or weekday) and calculate the mean value for each combination.  


```r
stepsPerTimeByFac <- aggregate(steps ~ time+weekFactor, dataImpute, mean)

stepsPerTimeByFac$time<- as.factor(stepsPerTimeByFac$time)
```

We can now plot the data, showing two plots - one for each day type.


```r
par(mfrow=c(1,2))

with(stepsPerTimeByFac[stepsPerTimeByFac$weekFactor == "weekday",], plot(time, steps, type="n", xlab="Time", ylab="Number of Steps", main="Weekday: Steps by Time Interval"))
with(stepsPerTimeByFac[stepsPerTimeByFac$weekFactor == "weekday",], lines(time, steps, type="l"))

with(stepsPerTimeByFac[stepsPerTimeByFac$weekFactor == "weekend",], plot(time, steps, type="n", xlab="Time", ylab="Number of Steps", main="Weekend: Steps by Time Interval"))
with(stepsPerTimeByFac[stepsPerTimeByFac$weekFactor == "weekend",], lines(time, steps, type="l"))
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

From these plots it's clear that the number of steps for Weekdays is concentrated early in the morning - presumably a walk to work. There is much less of a clear spkie towards the end of the day - perhaps reflecting a variety of finish times and destinations after work (i.e. shops, gym, pub etc rather than stright home).   

The Weekend data shows a much higher average number of steps throughout the day and no large spike in the data in the morning. The weekend is clearly a more varied pattern with the potential for a number of activities taking place, rather than the repetitive, predictable commute seen in the Weekday data. 




