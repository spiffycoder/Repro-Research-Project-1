# Reproducible Research
# Project 1

# Load required libraries:
library(ggplot2)
library(xtable)

# Load the activity data:
df.i <- read.csv("activity.csv")

# Check the data:
h.noproc <- head(df.i, 5)
h.noproc <- xtable(h.noproc, caption="First 5 rows: non-processed", label="Head Xtable", digits=1)
print(h.noproc, include.rownames = TRUE, caption.placement="top")

# preliminary processing of the data:
# ignore missing values:
df <- na.omit(df.i)
h.proc <- head(df, 5)
h.proc <- xtable(h.proc, caption="First 5 rows: processed", label="HeadP Xtable", digits=1)
print(h.proc, include.rownames = TRUE, caption.placement="top")

## What is mean total number of steps taken per day?
df.steps <- aggregate(steps ~ date, df, sum)

# Histogram of the total number of steps taken each day
hist(df.steps$steps, col="lightgreen", main = "Histogram of Total Steps Taken Each Day", 
     xlab="Total Number of Steps in a Day")

# Calculate mean and median steps per day:
mean(df.steps$steps)
median(df.steps$steps)

##New Bins set
#qplot(steps, data=df.steps, binwidth = "1", xlab = "Total number of steps taken each day", 
#      main = "Steps with binwidth set at 1", na.rm=TRUE) + 
#      geom_histogram(colour="darkgreen", aes(fill = ..count..)) 

## What is the average daily activity pattern?
library(ggplot2)
df.averages <- aggregate(x=list(steps=df$steps), by=list(interval=df$interval), FUN=mean)
ggplot(data=df.averages, aes(x=interval, y=steps)) + geom_line() + 
  xlab("Intervals set at 5 minutes") + ylab("Average of steps taken")

# Which interval has most steps:
df.averages[which.max(df.averages$steps),]

## Imputing missing values

# Find how much we're missing:
df.missing <- is.na(df$steps)
num.missing <- sum(df.missing)
table(df.missing)
table (num.missing)

# replace the missing values with the mean value of the 5-minute intervals
nafiller <- function(steps, interval){
  filler <- NA
  if (!is.na(steps))
    filler <- c(steps)
  else
    filler <- (df.averages[df.averages$interval==interval, "steps"])
  return(filler)
}
myfill.df <- df
myfill.df$steps <- mapply(nafiller, myfill.df$steps, myfill.df$interval)

head(myfill.df)

# New historgram:
myts <- tapply(myfill.df$steps, myfill.df$date)
qplot(myts, binwidth=5, xlab="Total Number of Steps per Day",
      main="Total Number of Steps per Day After Imput")

library(psych)

describe(myts)
mean(myts)
median(myts)
summary(myts)

# Compare differences in activity patterns between weekdays and weekends
week.identify <- function(date){
 day <- weekdays(date)
 if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
   return("Weekday")
 else if (day %in% c("Saturday", "Sunday"))
   return("Weekend")
 else
   stop("Invalid Date")
}  
myfill.df$date <- as.Date(myfill.df$date)
myfill.df$day <- sapply(myfill.df$date, FUN=week.identify)

head(myfill.df$day)
avg <- aggregate(steps ~ interval + day, data=myfill.df, mean)
ggplot(avg, aes(interval, steps))+geom_line()+ facet_grid(day ~ .) + xlab("Intervals at 5 minutes") + ylab("# of Steps")

# Yes, there is a difference.

