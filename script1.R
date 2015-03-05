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
