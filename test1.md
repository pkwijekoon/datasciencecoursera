---
Title: "Introduction"  
date: "2015-10-22"  
output: pdf_document  
---
Introduction
========================================================
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a [Fitbit](http://www.fitbit.com/), [Nike Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuel), or [Jawbone Up](https://jawbone.com/up). These type of devices are part of the "quantified self" movement -- a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

---
## **Data**  
The data for this assignment can be downloaded from the course web site:  
- **Dataset**: [Activity monitoring data](https://d396qusza40orc.cloudfrony.net) [52K]

The variables included in this dataset are:  
- **steps**: Numberof steps taking in a 5-minute interval (missing values are coded as NA)
- **date**: The date on which the measurement was taken in YYYY-MM-DD format
- **interval**: Identifier for the 5-minute interval in which measurement was taken  

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

---

```r
activity=read.csv("D:/coursera/Reproducible Research/peer assessment/activity.csv")
nrow(activity)
```

```
## [1] 17568
```

```r
# create new dataset without missing data
newactivity <- na.omit(activity)
head(newactivity)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```

```r
#Total number of steps per each day
newdata=aggregate(steps ~ date, data = newactivity, sum)
#Mean of steps per each day
a=aggregate(steps ~ date, data = newactivity, mean)
#Mean of steps per day
mean(a$steps)
```

```
## [1] 37.3826
```

```r
#Median of steps per day
median(a$steps)
```

```
## [1] 37.37847
```
The mean and median of steps variable per day are 37.3826 and 37.37847, respectively.  

```r
#Creating a histogram per each day
jpeg('hplot.jpg')
hist(newdata$steps, breaks=5)
dev.off()
```

```
## png 
##   2
```

```r
histinfo<-hist(newdata$steps, breaks=5)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
histinfo
```

```
## $breaks
## [1]     0  5000 10000 15000 20000 25000
## 
## $counts
## [1]  5 12 28  6  2
## 
## $density
## [1] 1.886792e-05 4.528302e-05 1.056604e-04 2.264151e-05 7.547170e-06
## 
## $mids
## [1]  2500  7500 12500 17500 22500
## 
## $xname
## [1] "newdata$steps"
## 
## $equidist
## [1] TRUE
## 
## attr(,"class")
## [1] "histogram"
```
The highest frequency (28) of total number of steps per day occurs in the range 10000-15000, and the lowest frequency (2) occurs in the range 20000-25000.   

```r
#Creating time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days
b=aggregate(steps ~ interval, data = newactivity, mean)
head(b)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
summary(b)
```

```
##     interval          steps        
##  Min.   :   0.0   Min.   :  0.000  
##  1st Qu.: 588.8   1st Qu.:  2.486  
##  Median :1177.5   Median : 34.113  
##  Mean   :1177.5   Mean   : 37.383  
##  3rd Qu.:1766.2   3rd Qu.: 52.835  
##  Max.   :2355.0   Max.   :206.170
```

```r
#jpeg('rplot.jpg')
plot(b$interval,b$steps, type="l", main="Scatterplot of interval vs. mean number of steps", xlab="Interval", ylab="Mean number of steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

Maximum mean number of steps 206.170 occurs at 805th interval.  

```r
#Finding the number of missing values in steps variable
x=activity$steps
sum(is.na(x))
```

```
## [1] 2304
```
Total number of missing values in steps variable is 2304.

```r
#Filling in all of the missing values in the dataset using the mean for that 5-minute interval, and create a new data set called nactivity.
nactivity=activity
nactivity$steps[is.na(nactivity$steps)] =b$steps
nrow(nactivity)
```

```
## [1] 17568
```

```r
#Total number of steps per each day in nactivity data
tanc=aggregate(steps ~ date, data = nactivity, sum)
#Mean of steps per each day
anc=aggregate(steps ~ date, data = nactivity, mean)
#Mean of steps per day
mean(anc$steps)
```

```
## [1] 37.3826
```

```r
#Median of steps per day
median(anc$steps)
```

```
## [1] 37.3826
```

```r
#Comparing total daily number of steps.
activity[is.na(activity)] = -1
ndata=aggregate(steps ~ date, data = activity, sum)
nrow(ndata);nrow(tanc)
```

```
## [1] 61
```

```
## [1] 61
```

```r
comp=cbind(ndata,tanc )
head(comp)
```

```
##         date steps       date    steps
## 1 2012-10-01  -288 2012-10-01 10766.19
## 2 2012-10-02   126 2012-10-02   126.00
## 3 2012-10-03 11352 2012-10-03 11352.00
## 4 2012-10-04 12116 2012-10-04 12116.00
## 5 2012-10-05 13294 2012-10-05 13294.00
## 6 2012-10-06 15420 2012-10-06 15420.00
```
In this case both mean and median are 37.3826,which is the mean of the activity data without missing values. After imputing estimates to missing data, the totals are same for other steps, but for the missing values the total steps is the same which is 10766.19.

```r
#Separating week days and weekends in nactivity data

#m=weekdays(nactivity$date, abbr = TRUE)
mactivity=nactivity
mactivity$day <- weekdays(as.Date(mactivity$date))
#mactivity
#mactivity$day<-strptime(mactivity[,2], "%Y-%m-%d")
#mactivity$weekday<-weekdays(mactivity$day)
#Creating new factor variables df1 and df2 in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day
df1=subset(mactivity,mactivity$day=="Saturday" | mactivity$day == "Sunday")
df2=subset(mactivity,mactivity$day!="Saturday" & mactivity$day != "Sunday")
head(df1) ; head(df2)
```

```
##      steps       date interval      day
## 1441     0 2012-10-06        0 Saturday
## 1442     0 2012-10-06        5 Saturday
## 1443     0 2012-10-06       10 Saturday
## 1444     0 2012-10-06       15 Saturday
## 1445     0 2012-10-06       20 Saturday
## 1446     0 2012-10-06       25 Saturday
```

```
##       steps       date interval    day
## 1 1.7169811 2012-10-01        0 Monday
## 2 0.3396226 2012-10-01        5 Monday
## 3 0.1320755 2012-10-01       10 Monday
## 4 0.1509434 2012-10-01       15 Monday
## 5 0.0754717 2012-10-01       20 Monday
## 6 2.0943396 2012-10-01       25 Monday
```
Two data sets called df1 and df2 are created which separate two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
#Finding 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days 
b1=aggregate(steps ~ interval, data = df1, mean)
b2=aggregate(steps ~ interval, data = df2, mean)
#Adding a factor variable to each b1 and b2 as weekends and weekdays
b1["Time"] <- "Weekend" 
b2["Time"] <- "Weekdays" 
tdat=rbind(b1,b2)

#Creating panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days
library("lattice")
xyplot(steps ~ interval| Time,data = tdat,type = "l", main="Panal plot showing the differences in activity patterns between weekdays and weekends", xlab="Interval", ylab="Mean number of steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

According to the panal plot there are differences in activity patterns between weekdays and weekends.


