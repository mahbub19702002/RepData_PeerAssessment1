---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
'''{r}
library(ggplot2)
activity <- read.csv(unz('activity.zip', 'activity.csv'), header = TRUE)
activity <- na.omit(activity)
'''


## What is mean total number of steps taken per day?
'''{r}
activity$date <- as.Date(activity$date)
total_df <- aggregate(activity$steps, by=list(activity$date), sum)
colnames(total_df) <- c('Date', 'Total Steps')
ggplot(data = total_df, aes(total_df$`Total Steps`)) + geom_histogram(col = 'red', fill = 'green', alpha = 0.2) + labs(title = 'Total Number of Steps Taken Per Day') + labs(x = 'Total Steps per Day', y = 'Number of Days') + ylim(c(0, 10))
mean_steps = mean(total_df$`Total Steps`, na.rm = TRUE)
median_steps = median(total_df$`Total Steps`, na.rm = TRUE)
'''


## What is the average daily activity pattern?
'''{r}
mean_df <- aggregate(activity$steps, by=list(activity$interval), mean)
colnames(mean_df) <- c('Interval', 'Mean Steps')
ggplot(mean_df, aes(x = mean_df$Interval, y = mean_df$`Mean Steps`)) + geom_line(col = 'blue', size = 1) + labs(title = 'Time Series of the Mean Number of Steps') + labs(x = 'Number of 5-Minute Intervals', y = 'Mean Number of Steps') + ylim(c(0, 250))
maximum_average = mean_df[which.max(mean_df$`Mean Steps`),]

The following interval of 5-minutes contains the maximum average number of steps
maximum_average[,'Interval']
'''


## Imputing missing values

The number of missing values is:
'''{r}
activity_orig <- read.csv(unz('activity.zip', 'activity.csv'), header = TRUE)
activity_no_na <- na.omit(activity)
number_of_NA_rows <- nrow(activity_orig) - nrow(activity_no_na)
'''

All the missing values are filled with the average number of steps per day.

'''{r}
activity_orig$date <- as.Date(activity_orig$date)
mean_steps_to_replace <- mean(activity_orig$steps, na.rm = TRUE)
activity_orig$steps <- ifelse(is.na(activity_orig$steps) == TRUE, mean_steps_to_replace, activity_orig$steps)
total_imputed_df <- aggregate(activity_orig$steps, by=list(activity_orig$date), sum)
colnames(total_imputed_df) <- c('Date', 'Total Steps')
ggplot(data = total_imputed_df, aes(total_imputed_df$`Total Steps`)) + geom_histogram(col = 'red', fill = 'green', alpha = 0.2) + labs(title = 'Total Number of Steps Taken Per Day') + labs(x = 'Total Steps per Day', y = 'Number of Days') + ylim(c(0, 10))
mean_imputed_steps = mean(total_imputed_df$`Total Steps`)
median_imputed_steps = median(total_imputed_df$`Total Steps`)
'''


## Are there differences in activity patterns between weekdays and weekends?
'''{r}
daytype <- function(date) {
    day <- weekdays(date)
    
    day_type <- ifelse( day %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday') == TRUE, 'weekday', 'weekend')
    
    return (day_type)
}

activity_orig$day <- sapply(activity_orig$date, FUN = daytype)
avg_imputed_df <- aggregate(steps ~ interval + day, data = activity_orig, mean)
ggplot(avg_imputed_df, aes(interval, steps)) + geom_line(col = 'red', size = 1) + facet_grid(day ~ .) + labs(title = 'Time Series of the Mean Number of Steps') + labs(x = 'Number of 5-Minute Intervals', y = 'Mean Number of Steps') + ylim(c(0, 250))
'''
