---
title: "Reproducible research: Project1"
output: 
  html_document:
    keep_md: true
author: Zine Eddine
date: 12/05/2018
---


```r
knitr::opts_chunk$set(echo = TRUE)
```

## Loading and processing 

We download the data from the website and then we save it into our directory. After this, we run the below code.

First, we include the packages that we are going to use: ggplot2,dplyr and chron.


```r
library(dplyr)
library(ggplot2)
library(chron)
```

After this, we start to read into our file.


```r
data_raw1 <- read.csv("activity.csv",stringsAsFactors = FALSE)
head(data_raw1)
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

```r
tail(data_raw1)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```

```r
str(data_raw1)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
We see that we have some missing values and that the date column should be converted to date format. 


```r
data_raw1$date <- as.Date(data_raw1$date)
data_raw <- data_raw1[complete.cases(data_raw1),]
```
Now, we've got rid of all missing cases and we have the date column in a date format. 

Now, we start to construct the first plot: the histogram of steps for each day. 

```r
sp <- ggplot(data = data_raw,
       aes(date, steps)) +
        stat_summary(fun.y = sum, geom = "bar")
```
This will do the sum of steps for each day. We want to add to this the mean and median values as lines. Note that these two are so close in the first scenario(when we ignore the missing cases)

```r
data_raw_sum<- data_raw %>%
        group_by(date) %>%
        mutate(total=sum(steps))  

mean_val <- mean(data_raw_sum$total)
med_val <- median(data_raw_sum$total)
```
Now that we have what we need, we construct our plot for the first part.

```r
sp    +
 geom_line( aes(y =  mean_val,  color = "mean_val"), linetype="dashed", size=2) +
 geom_line(aes(y=med_val,color = "med_val")  ,linetype="dotdash", size=2) +
        scale_color_manual(name = "", 
                           values = c("mean_val" = "blue","med_val" = "red"))
```

![](analysis_files/figure-html/plot-1.png)<!-- -->

## Second part

Now we make the histogram of steps for each interval.


```r
ggplot(data = data_raw,
       aes(interval, steps)) +
        stat_summary(fun.y = mean,
                     geom = "line")
```

![](analysis_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
We see here that the interval that holds the maximum number of steps is around 800. Let's confirm this result.

```r
avg_5min <- data_raw %>%
        group_by(interval) %>%
        mutate(total=sum(steps))

(max_5min <- avg_5min[which.max(avg_5min$total),3])
```

```
## # A tibble: 1 x 1
## # Groups:   interval [1]
##   interval
##      <int>
## 1      835
```
## Third part

Before imputing missing values, let's first check how they are distributed in our data set:


```r
colSums(is.na(data_raw1))
```

```
##    steps     date interval 
##     2304        0        0
```
We see that only the column steps contains NA. We think it's useful to replace the NA values with the median of that day.

```r
data_raw <- data_raw1
data_raw$steps[which(is.na(data_raw$steps))] <- median(data_raw$steps,na.rm = TRUE)
```


After this,  we construct our plot in the same way as the first one.


```r
sp <- ggplot(data = data_raw,
       aes(date, steps)) +
        stat_summary(fun.y = sum,
                     geom = "bar")

data_raw_sum<- data_raw %>%
        group_by(date) %>%
        mutate(total=sum(steps))  

mean_val <- mean(data_raw_sum$total)
med_val <- median(data_raw_sum$total)

sp    +
 geom_line( aes(y =  mean_val,  color = "mean_val"), linetype="dashed", size=2)+
 geom_line(aes(y=med_val,color = "med_val")  ,linetype="dotdash", size=2)+
        scale_color_manual(name = "", 
                           values = c("mean_val" = "blue", "med_val" = "red"))
```

![](analysis_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
The plot shows that imputing NA values gives different results  than when we completely discard them. 

## Fourth part

To see the difference between weekdays and weekends, we construct a vector that labels weekend as "weekend" and weekdays as "weekdays" and we add it to our data set.


```r
weekend=ifelse(is.weekend(data_raw$date),"weekend","weekday")
data_raw <- ungroup(data_raw)
data_raw <- mutate(data_raw,weekend1=weekend)
```
After that, we make a plot with two facets: one for the weekdays, and the other for the weekend.


```r
ggplot(data = data_raw,
       aes(interval, steps,group=weekend1)) +
        stat_summary(fun.y = mean,geom = "line",color="blue")+ facet_grid( data_raw$weekend1~.)
```

![](analysis_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
We see that the activity is different between these two.
