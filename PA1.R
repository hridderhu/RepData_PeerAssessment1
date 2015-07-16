# de commando's doen
setwd("H:/Coursera-Datascientist/05-RepData/Assignment1/git-repo")
#
activity1 <- read.csv(unz("activity.zip", "activity.csv"))
#
# Calculate total number of steps per day
stepsperday1 <- aggregate(steps ~ date, data = activity1, sum)
# make histogram
hist(stepsperday1$steps, 
     col="lightblue", 
     main="Histogram of steps per day",
     xlab="Steps per day",
     ylab="Frequency")

#cat ("Press [enter] to continue")
#line <- readline()

# summary  ?? 
summarystepsperday1 <- summary(stepsperday1$steps)
print (summarystepsperday1["Median"])
print (summarystepsperday1["Mean"])
#
# print tabel with mean and median
# print(aggregate(steps ~ date, data = activity, mean ))
# print(aggregate(steps ~ date, data = activity, median))
# aggregate(steps ~ date, data = activity, summary)
#
stepsperinterval1 <- aggregate(steps ~ interval, data = activity1, mean )
plot(stepsperinterval1$interval, stepsperinterval1$steps, xlab= "Day-interval (5-minute)", ylab= "Average nr of steps taken", type='l', col='red')
title(main="Averaged across all days", col.main="black", font.main=4)
#
# Maximum interval
print (stepsperinterval1[ stepsperinterval1$steps == max( stepsperinterval1$steps), ])
#
# nr of na's
print (sum(is.na(activity1$steps)))
#
# function for replacing na's with mean of 5-minute interval
replacena <- function(x,interval) {
  if (is.na(x))
    round(stepsperinterval1[stepsperinterval1$interval == interval,][,2])
  else
    x
}
# copy activity
activity2 <- activity1
# repalce na's
activity2$steps <- mapply(replacena,activity2$steps,activity2$interval)
#
stepsperday2 <- aggregate(steps ~ date, data = activity2, sum)
# make histogram
hist(stepsperday2$steps, 
     col="lightgreen", 
     main="Histogram of corrected steps per day",
     xlab="Steps per day",
     ylab="Frequency")
# summary  ?? 
summarystepsperday2 <- summary(stepsperday2$steps)
print (summarystepsperday2["Median"])
print (summarystepsperday2["Mean"])
#
makeweekfactor <- function(datestr) {
  aday <- as.Date(datestr)
  wday <- weekdays(aday,abbreviate=TRUE)
  if (wday == "za" || wday == "zo") { 
    1
  } else { 
    0
  }
}
activity2$day <- sapply(activity2$date,makeweekfactor)
activity2$day <- factor(activity2$day,labels = c("weekday","weekend"))

stepsperinterval2 <- aggregate(steps ~ interval + day, data = activity2, mean )
par(mfrow = c(2,1))
plot(subset(stepsperinterval2,day=="weekend")$interval, 
     subset(stepsperinterval2,day=="weekend")$steps, 
     xlab= "Day-interval (5-minute)", 
     ylab= "Average nr of steps taken", 
     main="weekend",
     type='l', 
     col='red')
plot(subset(stepsperinterval2,day=="weekday")$interval, 
     subset(stepsperinterval2,day=="weekday")$steps, 
     xlab= "Day-interval (5-minute)", 
     ylab= "Average nr of steps taken", 
     main="weekday",
     type='l', 
     col='red')
