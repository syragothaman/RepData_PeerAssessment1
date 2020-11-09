---
  title: 'Reproducible Research: Course Project 1'
author: "Sowmya Ragothaman"
date: "11/9/2020"
output: html_document
---

data <- read.csv("./activity.csv", header = TRUE)

### What is the mean total number of steps taken per day?

totalstepsperday <- aggregate(steps ~ date, data, FUN=sum)
totalstepsperday

hist(totalstepsperday$steps, xlab = "Number of Steps", main = "Histogram of Total Steps Per Day")

meanstepsperday <- mean(totalstepsperday$steps, na.rm = TRUE )
meanstepsperday
medianstepsperday <- median(totalstepsperday$steps, na.rm = TRUE)
medianstepsperday

### What is the average daily activity pattern?

library(ggplot2)
intervalplot <- aggregate(steps ~ interval, data, FUN=mean)
ggplot(data = intervalplot, aes(x = interval, y = steps)) +
  geom_line() +
  xlab("5-minute Interval") +
  ylab("Average Number of Steps") +
  ggtitle("Average Daily Activity Pattern") +
  theme(plot.title = element_text(hjust = 0.5))

Max_Steps_of_All_Intervals <- intervalplot[which.max(intervalplot$steps),]
Max_Steps_of_All_Intervals

### Imputing Missing Values


missingvalues <- is.na(data$steps)
str(missingvalues)

imputeddata <- transform(data, steps = ifelse(is.na(data$steps), intervalplot$steps[match(data$interval, intervalplot$interval)], data$steps))

impstepsinterval <- aggregate(steps ~ date, imputeddata, FUN = sum)
hist(impstepsinterval$steps,
     main = "Imputed Number of Steps Per Day",
     xlab = "Number of Steps")

impmeanSteps <- mean(impstepsinterval$steps, na.rm = TRUE)
impmedSteps <- median(impstepsinterval$steps, na.rm = TRUE)

diffTotal = sum(impstepsinterval$steps) - sum(totalstepsperday$steps)

### Are there differences in activity patterns between weekdays and weekends?

daytype <- function(date) {
  day <- weekdays(date)
  if (day %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))
      return ("weekday")
  else if (day %in% c('Saturday', 'Sunday'))
      return ("weekend")
  else
      stop ("Invalid Date Format.")

imputeddata$date <- as.Date(imputeddata$date)
imputeddata$day <- sapply(imputeddata$date, FUN = daytype)

meanstepsbydaytype <- aggregate(steps ~ interval + day, imputeddata, mean)
ggplot(data = meanstepsbydaytype, aes(x = interval, y = steps)) + 
  geom_line() +
  facet_grid(day ~ .) +
  xlab("5-minute Interval") +
  ylab("Average Number of Steps") +
  ggtitle("Average Daily Activity Pattern") +
  theme(plot.title = element_text(hjust = 0.5))
