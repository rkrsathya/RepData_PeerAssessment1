# PA1_template



## Requirements

This document helps to read the activity data of anonymous user for two months from csv file and generate plots on various evaluation conditions.

The variables included in this dataset are:

    1. steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
    2. date: The date on which the measurement was taken in YYYY-MM-DD format
    3. interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

1. Read the data from csv file


```r
activityFrame <- read.csv("activity/activity.csv")
```

2. Histogram of the total number of steps taken each day.

Bar plot helps in plotting the categoral data with the respective numerical column in y axis. But histogram plots continuous scale in x axis along with frequencies and density in y axis. Also histogram generates bin with equal width of bars generated.


```r
library(ggplot2)
activityNoNA <- na.omit(activityFrame)
activityAggregation <- aggregate(activityNoNA$steps~activityNoNA$date,activityNoNA,FUN = function(x) c(total = sum(x),mean = mean(x),medians = median(x)))
names(activityAggregation) <- c("date","steps")

ggplot(data = activityAggregation,aes(x=date,y=steps[,"total"]))+ labs(x="Date",y="Steps")+geom_bar(stat = "Identity")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

3. Mean and median number of steps taken each day.

The below table columns "steps.mean" and "steps.median" for each date.

```r
activityAggregation
```

```
##          date  steps.total   steps.mean steps.medians
## 1  2012-10-02 1.260000e+02 4.375000e-01  0.000000e+00
## 2  2012-10-03 1.135200e+04 3.941667e+01  0.000000e+00
## 3  2012-10-04 1.211600e+04 4.206944e+01  0.000000e+00
## 4  2012-10-05 1.329400e+04 4.615972e+01  0.000000e+00
## 5  2012-10-06 1.542000e+04 5.354167e+01  0.000000e+00
## 6  2012-10-07 1.101500e+04 3.824653e+01  0.000000e+00
## 7  2012-10-09 1.281100e+04 4.448264e+01  0.000000e+00
## 8  2012-10-10 9.900000e+03 3.437500e+01  0.000000e+00
## 9  2012-10-11 1.030400e+04 3.577778e+01  0.000000e+00
## 10 2012-10-12 1.738200e+04 6.035417e+01  0.000000e+00
## 11 2012-10-13 1.242600e+04 4.314583e+01  0.000000e+00
## 12 2012-10-14 1.509800e+04 5.242361e+01  0.000000e+00
## 13 2012-10-15 1.013900e+04 3.520486e+01  0.000000e+00
## 14 2012-10-16 1.508400e+04 5.237500e+01  0.000000e+00
## 15 2012-10-17 1.345200e+04 4.670833e+01  0.000000e+00
## 16 2012-10-18 1.005600e+04 3.491667e+01  0.000000e+00
## 17 2012-10-19 1.182900e+04 4.107292e+01  0.000000e+00
## 18 2012-10-20 1.039500e+04 3.609375e+01  0.000000e+00
## 19 2012-10-21 8.821000e+03 3.062847e+01  0.000000e+00
## 20 2012-10-22 1.346000e+04 4.673611e+01  0.000000e+00
## 21 2012-10-23 8.918000e+03 3.096528e+01  0.000000e+00
## 22 2012-10-24 8.355000e+03 2.901042e+01  0.000000e+00
## 23 2012-10-25 2.492000e+03 8.652778e+00  0.000000e+00
## 24 2012-10-26 6.778000e+03 2.353472e+01  0.000000e+00
## 25 2012-10-27 1.011900e+04 3.513542e+01  0.000000e+00
## 26 2012-10-28 1.145800e+04 3.978472e+01  0.000000e+00
## 27 2012-10-29 5.018000e+03 1.742361e+01  0.000000e+00
## 28 2012-10-30 9.819000e+03 3.409375e+01  0.000000e+00
## 29 2012-10-31 1.541400e+04 5.352083e+01  0.000000e+00
## 30 2012-11-02 1.060000e+04 3.680556e+01  0.000000e+00
## 31 2012-11-03 1.057100e+04 3.670486e+01  0.000000e+00
## 32 2012-11-05 1.043900e+04 3.624653e+01  0.000000e+00
## 33 2012-11-06 8.334000e+03 2.893750e+01  0.000000e+00
## 34 2012-11-07 1.288300e+04 4.473264e+01  0.000000e+00
## 35 2012-11-08 3.219000e+03 1.117708e+01  0.000000e+00
## 36 2012-11-11 1.260800e+04 4.377778e+01  0.000000e+00
## 37 2012-11-12 1.076500e+04 3.737847e+01  0.000000e+00
## 38 2012-11-13 7.336000e+03 2.547222e+01  0.000000e+00
## 39 2012-11-15 4.100000e+01 1.423611e-01  0.000000e+00
## 40 2012-11-16 5.441000e+03 1.889236e+01  0.000000e+00
## 41 2012-11-17 1.433900e+04 4.978819e+01  0.000000e+00
## 42 2012-11-18 1.511000e+04 5.246528e+01  0.000000e+00
## 43 2012-11-19 8.841000e+03 3.069792e+01  0.000000e+00
## 44 2012-11-20 4.472000e+03 1.552778e+01  0.000000e+00
## 45 2012-11-21 1.278700e+04 4.439931e+01  0.000000e+00
## 46 2012-11-22 2.042700e+04 7.092708e+01  0.000000e+00
## 47 2012-11-23 2.119400e+04 7.359028e+01  0.000000e+00
## 48 2012-11-24 1.447800e+04 5.027083e+01  0.000000e+00
## 49 2012-11-25 1.183400e+04 4.109028e+01  0.000000e+00
## 50 2012-11-26 1.116200e+04 3.875694e+01  0.000000e+00
## 51 2012-11-27 1.364600e+04 4.738194e+01  0.000000e+00
## 52 2012-11-28 1.018300e+04 3.535764e+01  0.000000e+00
## 53 2012-11-29 7.047000e+03 2.446875e+01  0.000000e+00
```

4. Time series plot of the average number of steps taken.

Below plot generates time series for mean of steps across the interval for all days.


```r
activityInterval <- aggregate(steps~interval,activityNoNA,mean)
ggplot(data = activityInterval,aes(x=interval,y=steps))+geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

5. The 5-minute interval that, on average, contains the maximum number of steps.

The interval that has maximum number of steps is 

```r
activityInterval[which.max(activityInterval$steps),"interval"]
```

```
## [1] 835
```

6. Code to describe and show a strategy for imputing missing data.

The number of missing data in the dataset is

```r
nrow(activityFrame[is.na(activityFrame$steps),])
```

```
## [1] 2304
```

The missing data for steps in the dataset is imputed by taking average of all days for the interval.

```r
activityFrame$steps[is.na(activityFrame$steps)] <- activityInterval[match(activityFrame$interval[is.na(activityFrame$steps)],activityInterval$interval),"steps"]
```

Mean and median is calculated for the new dataset aross the date.


```r
activityAggregationIm <- aggregate(activityFrame$steps~activityFrame$date,activityFrame,FUN = function(x) c(total = sum(x),mean = mean(x),medians = median(x)))
names(activityAggregationIm) <- c("date","steps")
```

Histogram is generated for the new imputed dataset.


```r
ggplot(data = activityAggregationIm,aes(x=date,y=steps[,"total"]))+geom_bar(stat = "Identity")+ labs(x="Date",y="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

The histogram from the omitted missing data and imputed dataset shows high different pattern.The mean and medians of the imputed dataset is shown below


```r
activityAggregationIm
```

```
##          date  steps.total   steps.mean steps.medians
## 1  2012-10-01 1.076619e+04 3.738260e+01  3.411321e+01
## 2  2012-10-02 1.260000e+02 4.375000e-01  0.000000e+00
## 3  2012-10-03 1.135200e+04 3.941667e+01  0.000000e+00
## 4  2012-10-04 1.211600e+04 4.206944e+01  0.000000e+00
## 5  2012-10-05 1.329400e+04 4.615972e+01  0.000000e+00
## 6  2012-10-06 1.542000e+04 5.354167e+01  0.000000e+00
## 7  2012-10-07 1.101500e+04 3.824653e+01  0.000000e+00
## 8  2012-10-08 1.076619e+04 3.738260e+01  3.411321e+01
## 9  2012-10-09 1.281100e+04 4.448264e+01  0.000000e+00
## 10 2012-10-10 9.900000e+03 3.437500e+01  0.000000e+00
## 11 2012-10-11 1.030400e+04 3.577778e+01  0.000000e+00
## 12 2012-10-12 1.738200e+04 6.035417e+01  0.000000e+00
## 13 2012-10-13 1.242600e+04 4.314583e+01  0.000000e+00
## 14 2012-10-14 1.509800e+04 5.242361e+01  0.000000e+00
## 15 2012-10-15 1.013900e+04 3.520486e+01  0.000000e+00
## 16 2012-10-16 1.508400e+04 5.237500e+01  0.000000e+00
## 17 2012-10-17 1.345200e+04 4.670833e+01  0.000000e+00
## 18 2012-10-18 1.005600e+04 3.491667e+01  0.000000e+00
## 19 2012-10-19 1.182900e+04 4.107292e+01  0.000000e+00
## 20 2012-10-20 1.039500e+04 3.609375e+01  0.000000e+00
## 21 2012-10-21 8.821000e+03 3.062847e+01  0.000000e+00
## 22 2012-10-22 1.346000e+04 4.673611e+01  0.000000e+00
## 23 2012-10-23 8.918000e+03 3.096528e+01  0.000000e+00
## 24 2012-10-24 8.355000e+03 2.901042e+01  0.000000e+00
## 25 2012-10-25 2.492000e+03 8.652778e+00  0.000000e+00
## 26 2012-10-26 6.778000e+03 2.353472e+01  0.000000e+00
## 27 2012-10-27 1.011900e+04 3.513542e+01  0.000000e+00
## 28 2012-10-28 1.145800e+04 3.978472e+01  0.000000e+00
## 29 2012-10-29 5.018000e+03 1.742361e+01  0.000000e+00
## 30 2012-10-30 9.819000e+03 3.409375e+01  0.000000e+00
## 31 2012-10-31 1.541400e+04 5.352083e+01  0.000000e+00
## 32 2012-11-01 1.076619e+04 3.738260e+01  3.411321e+01
## 33 2012-11-02 1.060000e+04 3.680556e+01  0.000000e+00
## 34 2012-11-03 1.057100e+04 3.670486e+01  0.000000e+00
## 35 2012-11-04 1.076619e+04 3.738260e+01  3.411321e+01
## 36 2012-11-05 1.043900e+04 3.624653e+01  0.000000e+00
## 37 2012-11-06 8.334000e+03 2.893750e+01  0.000000e+00
## 38 2012-11-07 1.288300e+04 4.473264e+01  0.000000e+00
## 39 2012-11-08 3.219000e+03 1.117708e+01  0.000000e+00
## 40 2012-11-09 1.076619e+04 3.738260e+01  3.411321e+01
## 41 2012-11-10 1.076619e+04 3.738260e+01  3.411321e+01
## 42 2012-11-11 1.260800e+04 4.377778e+01  0.000000e+00
## 43 2012-11-12 1.076500e+04 3.737847e+01  0.000000e+00
## 44 2012-11-13 7.336000e+03 2.547222e+01  0.000000e+00
## 45 2012-11-14 1.076619e+04 3.738260e+01  3.411321e+01
## 46 2012-11-15 4.100000e+01 1.423611e-01  0.000000e+00
## 47 2012-11-16 5.441000e+03 1.889236e+01  0.000000e+00
## 48 2012-11-17 1.433900e+04 4.978819e+01  0.000000e+00
## 49 2012-11-18 1.511000e+04 5.246528e+01  0.000000e+00
## 50 2012-11-19 8.841000e+03 3.069792e+01  0.000000e+00
## 51 2012-11-20 4.472000e+03 1.552778e+01  0.000000e+00
## 52 2012-11-21 1.278700e+04 4.439931e+01  0.000000e+00
## 53 2012-11-22 2.042700e+04 7.092708e+01  0.000000e+00
## 54 2012-11-23 2.119400e+04 7.359028e+01  0.000000e+00
## 55 2012-11-24 1.447800e+04 5.027083e+01  0.000000e+00
## 56 2012-11-25 1.183400e+04 4.109028e+01  0.000000e+00
## 57 2012-11-26 1.116200e+04 3.875694e+01  0.000000e+00
## 58 2012-11-27 1.364600e+04 4.738194e+01  0.000000e+00
## 59 2012-11-28 1.018300e+04 3.535764e+01  0.000000e+00
## 60 2012-11-29 7.047000e+03 2.446875e+01  0.000000e+00
## 61 2012-11-30 1.076619e+04 3.738260e+01  3.411321e+01
```

7. Are there differences in activity patterns between weekdays and weekends?

The factor variables of weekdays and weekend are generated for the dataset date column.


```r
activityFrame$date <- as.Date(activityFrame$date)
activityFrame$week <- factor((weekdays(activityFrame$date) %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')), 
                             levels=c(FALSE, TRUE), labels=c('weekend', 'weekday') )
```
 
The time series plotted with panel facet grid for weekdays and weekend on averaged steps across the factor variables for intervals.


```r
activityWeekInterval <- aggregate(steps~week+interval,activityFrame,mean)

ggplot(data = activityWeekInterval,aes(x=interval,y=steps))+geom_line()+facet_grid(week~.)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

The final plot show there are activies lower in weekdays than weekend for the user, though there is a high peat at an interval for weekday.
