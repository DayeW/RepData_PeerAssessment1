#loading data
library(knitr)
library(ggplot2)
library(dplyr)

unzip("activity.zip")
activity <- read.csv("activity.csv")

head(activity)

#processing, nas omitted
activity.na <- na.omit(activity)

activity.na$date <- as.Date(activity.na$date)

#totalsteps and plot
total.steps <- tapply(activity.na$steps, activity.na$date, sum)

png("firstplot.png")
firstplot <- hist(total.steps, col = "green", main = "", 
     xlab = "Total Number of Steps per Day", breaks = 15)
print(firstplot)
dev.off()

mean(total.steps)

median(total.steps)

#steps/Interval and plot
activity.mean <- aggregate(x = list(steps = activity.na$steps),
                           by = list(interval = activity.na$interval), 
                           FUN = mean)
png("secondplot.png")
secondplot <- ggplot(activity.mean, aes(x = interval, y = steps)) +
  geom_line(color = "green", size = 0.5) + 
  theme_bw() +
  xlab("5 minute interval") +
  ylab("Average Number of Steps Across all Days")
print(secondplot)
dev.off()

maximuminterval <- activity.mean[which.max(activitymean$steps),]
maximuminterval

#NAs back
activitywithNA <- sum(is.na(activity$steps))
activitywithNA

#New dataset
mean.fill.interval <- function(steps, interval) {
  filled.interval <- NA
  if (!is.na(steps))
    filled.interval <- c(steps)
  else
    filled.interval <- (activity.mean[activity.mean$interval == interval, "steps"])
  return(filled.interval)
}
activity.fill <- activity
activity.fill$steps <- mapply(mean.fill.interval, activity.fill$steps, 
                              activity.fill$interval)
activity.steps <- tapply(activity.fill$steps, activity.fill$date, FUN = sum)

head(activity.fill)


#plot
png("thirdplot.png")
thirdplot <- hist(activity.steps, col = "green", main = "", 
                  xlab = "Total steps per day", breaks = 15)
print(thirdplot)
dev.off()

mean(activity.steps)
median(activity.steps)

# Date
activity.day <- function(date) {
  if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {"weekend"}
  else {"weekday"}
} 
activity.fill$day <- as.factor(sapply(activity.fill$date, activity.day))
head(activity.fill)

#Plot
activity.final <- aggregate(steps ~ interval + day, activity.fill, mean)
png("fourthplot.png")
fourthplot <- activity.final <- aggregate(steps ~ interval + day, activity.fill, mean)
ggplot(activity.final, aes(interval, steps)) + 
  geom_line(color = "green") + theme_bw() +
  facet_grid(day ~ .) + xlab("5 minute interval") + ylab("Number of steps")
print(fourthplot)
dev.off()
