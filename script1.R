# Load necessary libraries
library(dplyr)
library(lubridate)
library(RColorBrewer)

## prepare a color palette
accent <- brewer.pal(8,"Accent")

## Load and transform data

data <- read.csv("activity.csv")
data <- mutate(data, date = ymd(date))

## What is mean total number of steps taken per day?

## first get the total number of steps per day
by_date <- group_by(data,date)
total <- summarise(by_date,total_steps = sum(steps))

## plot a histogram
hist(total$total_steps, main="Histogram Steps/Day",xlab="Steps",breaks=10,col=accent[5])

## calculate the mean and median
mean(total$total_steps,na.rm=TRUE)
median(total$total_steps,na.rm=TRUE)

## What is the average daily activity pattern?
### group the data by inteval and find the mean of each interval
by_interval <- group_by(data,interval)
avs <- summarise(by_interval, average_steps = mean(steps,na.rm=TRUE))
plot(avs,pch=20,main="Average Steps for each Interval",ylab="Average Steps",col=accent[6])
lines(avs)


### find the interval that has the maximum number of steps
index <- which.max(avs$average_steps)
avs$interval[index]
