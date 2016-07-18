# Reproducible Research: Peer Assessment 1
(As taken from the course project description given for "Reproducible Research; Week 2: Project 1")

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012
and include the number of steps taken in 5 minute intervals each day.

## Data

The data for this assignment can be downloaded from the course web
site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this
dataset.

## Loading and preprocessing the data

Make sure that you are first in the directory containing the appropriate data file. In this case it is named "activity.csv". As a csv file we will be using "read.csv" to read it into our IDE.


```r
unzip(zipfile = "activity.zip")
activity_data <- read.csv("activity.csv", stringsAsFactors = FALSE)
```

## What is mean total number of steps taken per day?

We are asked the question of how many steps are taken within a 24 hour period for our given data set? To answer this we will construct a histogram showing the total amount of steps taken in one day on the x-axis. There will be a set of ranges for the total amount of steps taken, and we will get the frequency that a certain amount of steps are taken in a day on the y-axis. 

Dplyr is a good package to simplify our efforts.

```r
suppressMessages(require(dplyr))
require("dplyr", quietly = TRUE)
```

NA values should be discarded from our analysis.


```r
activity_clean <- activity_data[complete.cases(activity_data), ]
```

Since we want total steps per day, we will group our data by date and then plot it.


```r
total_steps  <-  activity_clean %>% 
                 group_by(date) %>% 
                 summarise(tot_steps = sum(steps))

hist(total_steps$tot_steps, 
    main = "Total Steps Per Day",
    xlab = "Steps Per Day Totals",
    ylab = "Frequency",
    border = "black",
    col = cm.colors(5),
    las = 1,
    ylim = c(0, 30))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)

```r
mean(total_steps$tot_steps)
```

```
## [1] 10766.19
```

```r
median(total_steps$tot_steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

We'll use ggplot2 this time to figure out the average daily activity pattern; that is, average amount of steps taken in a given interval. 


```r
suppressMessages(library(ggplot2))
library(ggplot2)

activity_pattern <- aggregate(x = list(steps = activity_clean$steps), by = list(interval = activity_clean$interva), FUN = mean, na.rm = TRUE)

ggplot(data = activity_pattern, aes(x = interval, y = steps)) +
    geom_line() +
    xlab("Interval") +
    ylab("Average Steps Taken In Interval")
```

![](PA1_template_files/figure-html/average daily activity pattern-1.png)

To figure out the 5-minute interval containing the maximum number of steps we will use the "which.max()" function.


```r
activity_pattern[which.max(activity_pattern$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

Missing values can skew the data unfavorably in certain circumstances. It is often better to impute missing values so that a better estimate or analysis can be obtained. We will utilize a random uniform distribution which will be based off of the steps taken in "activity_data". Basically, we figure out the maximum and minimum number of steps in our original data set and make a uniform distribution between those two points with equal number of rows to the original set, then we will fill in NA values with our imputed values.


```r
set.seed(1111)
set_uniform <- floor(runif(nrow(activity_data), min = min(activity_data$steps, na.rm = TRUE), max = max(activity_data$steps, na.rm = TRUE) / 10))
```

We will get an index of NA values and replace them with our imputed values.


```r
activity_NA  <- which(is.na(activity_data$steps))
activity_data$steps[activity_NA]  <- set_uniform[activity_NA]
```

Using dplyr again, we will get the sum of steps and then plot the data.


```r
imputed_data <- activity_data %>%
                group_by(date) %>%
                summarise(tot_steps = sum(steps))

hist(imputed_data$tot_steps, 
    breaks = 10,
    main = "Total Steps Per Day",
    xlab = "Steps Per Day Totals",
    ylab = "Frequency",
    border = "black",
    col = cm.colors(10),
    las = 1,
    ylim = c(0, 25))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)

```r
mean(total_steps$tot_steps)
```

```
## [1] 10766.19
```

```r
median(total_steps$tot_steps)
```

```
## [1] 10765
```

## Are there differences in activity patterns between weekdays and weekends?

To compare activity on weekdays or weekends, we will first change the "date" variable in our data to be of "date" class.


```r
activity_data$date <- as.POSIXct(activity_data$date)
```

We will use an if statement to sort and sapply to a new variable column "day_type" our dataset. In this manner we can then begin to plot the data according to the day type.


```r
day_type <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("Weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("Weekend")
    else
        stop("invalid date")
}

activity_data$day_type  <- sapply(activity_data$date, FUN = day_type)
activity_data$day_type  <- as.factor(activity_data$day_type)
```

Here we will again summarize our factored and grouped data to get a total number of steps, so that we can plot our totals.


```r
day_type_steps <- activity_data %>% 
                  group_by(day_type, interval) %>% 
                  summarise(tot_step = sum(steps))
```

Plotting the graph in ggplot2.


```r
ggplot(day_type_steps, aes(interval, tot_step)) + geom_line(color = 'blue', size = 1) + facet_grid(day_type ~ .) + xlab("Interval") + ylab("Total Number Of Steps In Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)

We can see that, overall, there is much more activity during the weekday than the weekend.
