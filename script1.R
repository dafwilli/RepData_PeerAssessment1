# Load necessary libraries
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(Amelia)
library(gridExtra)
## prepare a color palette
accent <- brewer.pal(8,"Accent")

## Load and transform data
data <- read.csv("activity.csv")
data <- mutate(data, date = ymd(date))

## What is mean total number of steps taken per day?

### first get the total number of steps per day
by_date <- group_by(data,date)
total <- summarise(by_date,total_steps = sum(steps))

### plot a histogram
hist(total$total_steps, main="Histogram Steps/Day",xlab="Steps",breaks=10,col=accent[7])

### calculate the mean and median
mean(total$total_steps,na.rm=TRUE)
median(total$total_steps,na.rm=TRUE)

## What is the average daily activity pattern?
### group the data by inteval and find the mean of each interval
by_interval <- group_by(data,interval)
avs <- summarise(by_interval, average_steps = mean(steps,na.rm=TRUE))
plot(avs,pch=".",main="Average Steps for each Interval",ylab="Average Steps",col=accent[5])
lines(avs)


### find the interval that has the maximum number of steps
index <- which.max(avs$average_steps)
avs$interval[index]


## Imputing missing values
### use Amelia package to visualize missing vales
missmap(data)

### there are ony missins in steps - so count the number missing
sum(is.na(data$steps))

### impute the missing values in steps by using the mean for each 5 minute interval
new <- data$steps

for (n in 1:17568){
  if (is.na(data$steps[n])) {
    index <- which(avs$interval == data$interval[n])
    new[n] <- round(avs$average_steps[index])
  }
}

### create new dataframe with the imputed values
new_data <- data
new_data <- select(new_data,date,interval)
new_data <- mutate(new_data,steps = new)

### plot a histogram of the new_data and get the mean and median
new_by_date <- group_by(new_data,date)
new_total <- summarise(new_by_date,total_steps = sum(steps))
hist(new_total$total_steps, main="Histogram Steps/Day",xlab="Steps",breaks=10,col=accent[7])

mean(new_total$total_steps)
median(new_total$total_steps)

## Are there differences in activity patterns between weekdays and weekends?
### make a new vector with weekday or weekend and add it to new_data dataframe
w_end <- c(1,7)
wk_day <- ifelse(wday(new_data$date) %in% w_end,"weekend","weekday")
new_data <- mutate(new_data, day = as.factor(wk_day))

### now group by interval and day and get the mean over the 5 intervals minute
by_int_day <- group_by(new_data, interval, day)
ave_interval_by_int_day <- summarise(by_int_day,ave_steps = mean(steps))
w_days <- filter(ave_interval_by_int_day, day == "weekday")
w_ends <- filter(ave_interval_by_int_day, day == "weekend")

### now plot them
p1 <- qplot(interval,ave_steps,data=w_days,geom="line")
p1 <- p1 + labs(title="Weekdays",x="",y="Average Steps")

p2 <- qplot(interval,ave_steps,data=w_ends,geom="line")
p2 <- p2 + labs(title="Weekends",x="Interval",y="Average Steps")

grid.arrange(p1,p2,ncol=1)