library(ggplot2)

# Change current working directory to the directory containing the zipped data file
#setwd("../Desktop/ERDC Data Science/Johns Hopkins Univertisy/5. Reproducible Research/RepData_PeerAssessment1/")

# Extract the .csv file from the zipped file
activity <- read.csv(unz('activity.zip', 'activity.csv'), header = TRUE)

# Remove all rows with NA values
activity <- na.omit(activity)

##################################################################################################################

##
## Mean total number of steps taken per day
##
# Convert the 'date' column from 'Factor' type into 'Date' type column
activity$date <- as.Date(activity$date)

# Calculate the total number of steps taken
total_df <- aggregate(activity$steps, by=list(activity$date), sum)
colnames(total_df) <- c('Date', 'Total Steps')

# Create a histogram from the aggregate data
ggplot(data = total_df, aes(total_df$`Total Steps`)) + geom_histogram(binwidth = 1000, col = 'red', fill = 'green', alpha = 0.2) + labs(title = 'Total Number of Steps Taken Per Day') + labs(x = 'Total Steps per Day', y = 'Number of Days') + ylim(c(0, 10))
mean_steps = mean(total_df$`Total Steps`, na.rm = TRUE)
median_steps = median(total_df$`Total Steps`, na.rm = TRUE)

##################################################################################################################

##
## Time series visualization of the activity data
##
mean_df <- aggregate(activity$steps, by=list(activity$interval), mean)
colnames(mean_df) <- c('Interval', 'Mean Steps')
ggplot(mean_df, aes(x = mean_df$Interval, y = mean_df$`Mean Steps`)) + geom_line(col = 'blue', size = 1) + labs(title = 'Time Series of the Mean Number of Steps') + labs(x = 'Number of 5-Minute Intervals', y = 'Mean Number of Steps') + ylim(c(0, 250))
maximum_average = mean_df[which.max(mean_df$`Mean Steps`),]
maximum_average[,'Interval']
maximum_average[,'Mean Steps']

##################################################################################################################
##
## Impute missing values
##

# Extract the .csv file from the zipped file
activity_orig <- read.csv(unz('activity.zip', 'activity.csv'), header = TRUE)

# Remove all rows with NA values
activity_no_na <- na.omit(activity)

# Number of rows with NA values
number_of_NA_rows <- nrow(activity_orig) - nrow(activity_no_na)

# Replace NA values in the original data frame with mean values
activity_orig$date <- as.Date(activity_orig$date)

# Get the average number of steps over the whole period of the experiment
mean_steps_to_replace <- mean(activity_orig$steps, na.rm = TRUE)

# Replace the NA values with the mean value for that day
activity_orig$steps <- ifelse(is.na(activity_orig$steps) == TRUE, mean_steps_to_replace, activity_orig$steps)

# Calculate the total number of steps taken
total_imputed_df <- aggregate(activity_orig$steps, by=list(activity_orig$date), sum)
colnames(total_imputed_df) <- c('Date', 'Total Steps')

# Create a histogram from the aggregate data
ggplot(data = total_imputed_df, aes(total_imputed_df$`Total Steps`)) + geom_histogram(binwidth = 1000, col = 'red', fill = 'green', alpha = 0.2) + labs(title = 'Total Number of Steps Taken Per Day') + labs(x = 'Total Steps per Day', y = 'Number of Days') + ylim(c(0, 10))
mean_imputed_steps = mean(total_imputed_df$`Total Steps`)
median_imputed_steps = median(total_imputed_df$`Total Steps`)


##################################################################################################################
##
## Activity pattern differences between weekdays and weekends
##

daytype <- function(date) {
    day <- weekdays(date)
    
    day_type <- ifelse( day %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday') == TRUE, 'weekday', 'weekend')
    
    return (day_type)
}

activity_orig$day <- sapply(activity_orig$date, FUN = daytype)

avg_imputed_df <- aggregate(steps ~ interval + day, data = activity_orig, mean)
ggplot(avg_imputed_df, aes(interval, steps)) + geom_line(col = 'red', size = 1) + facet_grid(day ~ .) + labs(title = 'Time Series of the Mean Number of Steps') + labs(x = 'Number of 5-Minute Intervals', y = 'Mean Number of Steps') + ylim(c(0, 250))





