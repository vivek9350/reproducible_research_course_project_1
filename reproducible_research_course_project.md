---
output: 
  html_document:
    keep_md: true
---

Reproducible Research: Peer Assessment 1
---

## Introduction

This report forms part of the [Reproducible Research course on Coursera](https://www.coursera.org/course/repdata). It presents an analysis on personal activity monitoring data. For more information, please see the [README](https://github.com/emilesilvis/RepData_PeerAssessment1).

## Data
The data for the analysis is sourced from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November in 2012. The data is available [here](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip). The variables included in the data are:

* steps: Number of steps taken in a 5-minute interval
* date: The date on which the measurement was taken in YYYY-MM-DD format
* interval: A unuiqe identifier for the 5-minute interval in which measurement was taken

Missing values are coded as "NA". The dataset contains 17,568 observations and is stored in a comma-sperated value (CSV) format.

## Obtaining the data

The data used for this analysis is contained in a file called `activity.csv`. If, however, this file is not present for some reason, the following code will download the file and unzip it:


```r
activity_data <- read.csv('./activity.csv')
```

The variables are converter to their correct representions in R:


```r
activity_data$date <- as.Date(activity_data$date)
activity_data$interval <- as.factor(activity_data$interval)
```

`activity_data` has the following variables of the following classes:


```r
names(activity_data)
```

```
## [1] "steps"    "date"     "interval"
```

```r
lapply(activity_data, class)
```

```
## $steps
## [1] "integer"
## 
## $date
## [1] "Date"
## 
## $interval
## [1] "factor"
```

## What is mean total number of steps taken per day?

To answer this question, a histogram will be produced for the total number of steps taken each day.

First the data for the plot is prepared by summing the number of steps for each day and removing any missing values (coded as `NA`):


```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 4.0.4
```

```r
analysis_1_data <- ddply(activity_data[,1:2], .(date), function(set) { sum(set$steps, na.rm = TRUE) })
```

`plot_data`'s variables are renamed to be friendlier:


```r
names(analysis_1_data) <- c("date", "steps")
```

The historgram is generated:


```r
library(ggplot2)
ggplot(data = analysis_1_data) + aes(x = factor(date), y = steps) + geom_histogram(stat = "identity") + labs(x ="Date", y = "Total number of steps") + theme(axis.text.x=element_text(angle = -90, hjust = 0))
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![](reproducible_research_course_project_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

The average and median number of total steps taken per day are:


```r
mean(analysis_1_data$steps)
```

```
## [1] 9354.23
```

```r
median(analysis_1_data$steps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

To answer this question, a time series plot will be produced for the average total number of steps taken for each interval across all days.

First the data for the plot is prepared by calculting the mean for the number of steps for each interval and removing any missing values (coded as `NA`):


```r
analysis_2_data <- ddply(activity_data, .(interval), function(set) { mean(set$steps, na.rm = TRUE) })
```

`plot_data`'s variables are renamed to be friendlier:


```r
names(analysis_2_data) <- c("interval", "steps")
```

The time series chart is generated:


```r
hour_intervals <- c(12, 24, 36, 48, 60, 72, 84, 96, 108, 120, 132, 144, 156, 168, 180, 192, 204, 216, 228, 240, 252, 264, 276)
ggplot(data = analysis_2_data) + aes(x = factor(interval), y = steps, group = 1) + geom_line() + labs(x ="5-minute interval", y = "Average number of steps across all days") + theme(axis.text.x = element_text(size = 0)) + geom_vline(xintercept= hour_intervals, linetype="dotted") + geom_vline(xintercept= 144, colour = "red") + geom_text(x=144, y = 150, label="12 pm", angle = 90)
```

![](reproducible_research_course_project_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

Each vertical dotted line denotes an hour. The red dotted line denotes the 12:00 mark (afternoon).

The inteval with the maximum number of steps is caluclated as follows:


```r
analysis_2_data[analysis_2_data$steps == max(analysis_2_data$steps), ]$interval
```

```
## [1] 835
## 288 Levels: 0 5 10 15 20 25 30 35 40 45 50 55 100 105 110 115 120 125 ... 2355
```

## Imputing missing values

There are 2304 missing values for the `steps` variable (coded as `NA`) in `activity_data`:


```r
nrow(activity_data[is.na(activity_data$steps), ])
```

```
## [1] 2304
```

To answer this question, a histogram will be produced for the total number of steps taken each day with missing values (coded `NA`) *inputed*. Missing values will be imputed by calculating the average number of steps for that interval across all days.

The function that will be used to impute looks as follows:

```r
impute_mean <- function(x) {
  replace(x, is.na(x), mean(x, na.rm = TRUE))
}
```

An `order` variable, representing the row order, is added to `activity_data` so that the data frame with the imputed values can be sorted according to the original order:


```r
activity_data$order <- 1:nrow(activity_data)
```

`NA` values are imputed by taking the mean of the total steps for that interval across all days. The resulting dataset is stored in `imputed_activity_data`:


```r
imputed_activity_data <- ddply(activity_data, .(interval), transform, steps = impute_mean(steps))
```

`imputed_activity_data` is ordered so that it reflects the original order of `activity_data`:


```r
imputed_activity_data <- imputed_activity_data[order(imputed_activity_data$order), ]
```

`imputed_activity_data` has the following variables of the following classes:


```r
names(activity_data)
```

```
## [1] "steps"    "date"     "interval" "order"
```

```r
lapply(activity_data, class)
```

```
## $steps
## [1] "integer"
## 
## $date
## [1] "Date"
## 
## $interval
## [1] "factor"
## 
## $order
## [1] "integer"
```

Now that missing values have been imputed, a histogram can be produced for the total number of steps taken each day. First the data for the plot is prepared by summing the number of steps for each day:


```r
analysis_3_data <- ddply(imputed_activity_data[,1:2], .(date), function(set) { sum(set$steps, na.rm = TRUE) })
```

`plot_data`'s variables are renamed to be friendlier:


```r
names(analysis_3_data) <- c("date", "steps")
```

The historgram is generated:


```r
ggplot(data = analysis_3_data) + aes(x = factor(date), y = steps) + geom_histogram(stat = "identity") + labs(x ="Date", y = "Total number of steps (NA imputed)") + theme(axis.text.x=element_text(angle = -90, hjust = 0))
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![](reproducible_research_course_project_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

The average and median number of total steps taken per day, with missing values imputed, are:


```r
mean(analysis_3_data$steps)
```

```
## [1] 10766.19
```

```r
median(analysis_3_data$steps)
```

```
## [1] 10766.19
```

The following table compares the values for `steps` where missing are present and where missing values have been imputed:

|             | With missing values                                   | Missing values imputed                                |
| ---         | ---                                                   |---                                                    | 
| **Mean**    | 9354   | 10766   |
| **Median**  | 10395 | 10766 |

The following observations are made:

* Both the mean and median are higher for the case where missing values have been imputed.
* For the case where missing values have been imputed, the mean and median are equal. This is due to the imputation strategy that was followed.

## Are there differences in activity patterns between weekdays and weekends?

To analyse activity patterns between weekdays and weekends, a vector is created with the weekday for each value of `date` in `imputed_activity_data`. A factor variable, `which_day`, is then added to `imputed_activity_data` indicating whether the date is on a weekday or a weekend


```r
which_day <- weekdays(imputed_activity_data$date)
imputed_activity_data$which_day <- ifelse(which_day == "Saturday" | which_day == "Sunday" , c("weekend"), c("weekday"))
imputed_activity_data$which_day <- factor(imputed_activity_data$which_day)
```

`imputed_activity_data` now has the following variables of the following classes:


```r
names(imputed_activity_data)
```

```
## [1] "steps"     "date"      "interval"  "order"     "which_day"
```

```r
lapply(imputed_activity_data, class)
```

```
## $steps
## [1] "numeric"
## 
## $date
## [1] "Date"
## 
## $interval
## [1] "factor"
## 
## $order
## [1] "integer"
## 
## $which_day
## [1] "factor"
```

Two time series plots will be produced:

* The average total number of steps taken for each interval across weekdays.
* The average total number of steps taken for each interval across weekends.

The data for the first plot is prepared by calculting the mean for the number of steps for each interval observed over **weekdays**:


```r
analysis_4_data_weekdays <- ddply(imputed_activity_data[imputed_activity_data$which_day == "weekday", ], .(interval), function(set) { mean(set$steps, na.rm = TRUE) })
```

`analysis_4_data_weekdays`'s variables are renamed to be friendlier:


```r
names(analysis_4_data_weekdays) <- c("interval", "steps")
```

The data for the second plot is prepared by calculting the mean for the number of steps for each interval observed over **weekdends**:


```r
analysis_4_data_weekends <- ddply(imputed_activity_data[imputed_activity_data$which_day == "weekend", ], .(interval), function(set) { mean(set$steps, na.rm = TRUE) })
```

`analysis_4_data_weekends`'s variables are renamed to be friendlier:


```r
names(analysis_4_data_weekends) <- c("interval", "steps")
```

Both time series chart objects are generated:


```r
plot_weekdays <- ggplot(data = analysis_4_data_weekdays) + aes(x = factor(interval), y = steps, group = 1) + geom_line() + labs(x ="5-minute interval", y = "Average number of steps across weekdays") + theme(axis.text.x = element_text(size = 0)) + geom_vline(xintercept= hour_intervals, linetype="dotted") + geom_vline(xintercept= 144, colour = "red") + geom_text(x=144, y = 150, label="12 pm", angle = 90)
plot_weekends <- ggplot(data = analysis_4_data_weekends) + aes(x = factor(interval), y = steps, group = 1) + geom_line() + labs(x ="5-minute interval", y = "Average number of steps across weekends") + theme(axis.text.x = element_text(size = 0)) + geom_vline(xintercept= hour_intervals, linetype="dotted") + geom_vline(xintercept= 144, colour = "red") + geom_text(x=144, y = 150, label="12 pm", angle = 90)
```

A function (taken from the R Cookbook at http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)) is defined to render both plots:


```r
multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
  
}
```

The plots are then generated using this function:


```r
multiplot(plot_weekdays, plot_weekends, cols = 1)
```

```
## Loading required package: grid
```

![](reproducible_research_course_project_files/figure-html/unnamed-chunk-30-1.png)<!-- -->

The following observations are made:

* By visually inspecting the two charts, activity appears to be higher over weekends than over weekdays.
