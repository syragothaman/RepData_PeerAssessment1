---
title: 'Reproducible Research: Course Project 1'
author: "Sowmya Ragothaman"
date: "11/9/2020"
output: html_document
---
The goal of this project is to answer the project's questions by creating an R markdown document processed by knitr and submitted as an HTML file.

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

First, we have to import the data.

```{r}
data <- read.csv("./activity.csv", header = TRUE)
```

Now we have to answer the project's questions and analyze the data.

### What is the mean total number of steps taken per day?

1. Calculate the total number of steps taken per day.

```{r}
totalstepsperday <- aggregate(steps ~ date, data, FUN=sum)
totalstepsperday
```

2. Make a histogram of the total number of steps taken each day.

```{r}
hist(totalstepsperday$steps, xlab = "Number of Steps", main = "Histogram of Total Steps Per Day")
```

3. Calculate & report the mean and median of the total number of steps taken per day.

```{r}
meanstepsperday <- mean(totalstepsperday$steps, na.rm = TRUE )
meanstepsperday
medianstepsperday <- median(totalstepsperday$steps, na.rm = TRUE)
medianstepsperday
```

The mean and median total steps per day are 10,766 steps.

### What is the average daily activity pattern?

1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days. 

```{r}
library(ggplot2)
intervalplot <- aggregate(steps ~ interval, data, FUN=mean)
ggplot(data = intervalplot, aes(x = interval, y = steps)) +
  geom_line() +
  xlab("5-minute Interval") +
  ylab("Average Number of Steps") +
  ggtitle("Average Daily Activity Pattern") +
  theme(plot.title = element_text(hjust = 0.5))
```

2. The 5-minute interval across all days with the maximum number of steps is:

```{r}
Max_Steps_of_All_Intervals <- intervalplot[which.max(intervalplot$steps),]
Max_Steps_of_All_Intervals
```

### Imputing Missing Values

There are several NA values in the imported data set "data". 

1. Calculate and report the total number of missing values in the dataset.

```{r}
missingvalues <- is.na(data$steps)
str(missingvalues)
```

There are 17,568 missing values. 

2. Devise a strategy for filling in all of the missing values in the dataset --> I will fill NAs in with the mean of that day.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
imputeddata <- transform(data, steps = ifelse(is.na(data$steps), intervalplot$steps[match(data$interval, intervalplot$interval)], data$steps))
```

4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. 

```{r}
impstepsinterval <- aggregate(steps ~ date, imputeddata, FUN = sum)
hist(impstepsinterval$steps,
     main = "Imputed Number of Steps Per Day",
     xlab = "Number of Steps")

impmeanSteps <- mean(impstepsinterval$steps, na.rm = TRUE)
impmedSteps <- median(impstepsinterval$steps, na.rm = TRUE)
```

Do these values differ from the estimates from the first part of the assignment? --> No- the mean and median total number of steps per day are both 10,766. They equal the mean and median calculated earlier without the imputed values. 

What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
diffTotal = sum(impstepsinterval$steps) - sum(totalstepsperday$steps)
```

There is a difference of 86,130 steps in total steps between the two datasets because quantitatively I added more steps to the original data set. Therefore, the number of total steps will grow. However, the mean and median values didn't change because I imputed mean values into the NA spots. Imputing missing data can keep some statistics the same but can drastically change others. 

### Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the newer (imputed) dataset with two levels (weekday and weekend) that indicates whether a given day is a weekday or a weekend day.

```{r}
daytype <- function(date) {
  day <- weekdays(date)
  if (day %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))
      return ("weekday")
  else if (day %in% c('Saturday', 'Sunday'))
      return ("weekend")
  else
      stop ("Invalid Date Format.")
}

imputeddata$date <- as.Date(imputeddata$date)
imputeddata$day <- sapply(imputeddata$date, FUN = daytype)
```

Make a panel plot as a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days.

```{r}
meanstepsbydaytype <- aggregate(steps ~ interval + day, imputeddata, mean)
ggplot(data = meanstepsbydaytype, aes(x = interval, y = steps)) + 
  geom_line() +
  facet_grid(day ~ .) +
  xlab("5-minute Interval") +
  ylab("Average Number of Steps") +
  ggtitle("Average Daily Activity Pattern") +
  theme(plot.title = element_text(hjust = 0.5))
```