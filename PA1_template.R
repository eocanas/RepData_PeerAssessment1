## Libraries

library(ggplot2)


## Functions: 

## Function to replace each NA value with the mean value of its 5-minute interval
fillFunc <- function(steps, interval) {
        filled <- NA
        if (!is.na(steps))
                filled <- c(steps)
        else
                filled <- (averagedData[averagedData$interval==interval, "steps"])
        return(filled)}

## Function to define day of the week
dayDef <- function(date) {
        day <- weekdays(date)
        if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
                return("Weekday")
        else if (day %in% c("Saturday", "Sunday"))
                return("Weekend")
        else
                stop("Invalid entry")
}

## Handling data
download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "data.zip", method = "curl")
unzip("data.zip", overwrite = T)
data<-read.csv("activity.csv", header=T, colClasses="character")

## Preparation of data.frame
data$date<-as.Date(data$date, format="%Y-%m-%d")
data$steps<-as.numeric(data$steps)
data$interval<-as.numeric(data$interval)

## NA omision
data2<-data[!is.na(data$steps),]

## Make a histogram of the total number of steps taken each day

x<-lapply(split(data2$steps,data2$date), sum)
x<-stack(x)
x$ind<-as.Date(x$ind)
hist(x$values,
     col = "blue", 
     main="Steps per day", 
     xlab="Steps", 
     ylab="Frequency")

## Calculation of mean and median 
stepsPerDay <- aggregate(steps ~ date, data2, sum)$steps
mean(stepsPerDay)
median(stepsPerDay)

## Average daily activity 

averagedData <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),FUN=mean, na.rm=TRUE)
ggplot(data=averagedData, aes(x=interval, y=steps)) + 
        geom_area() +xlab("Interval of 5 minutes each") +
        ylab("Average of steps taken")

## Interval with max value
averagedData[which.max(averagedData$steps),]


## NA values and result
valueNA <- is.na(data$steps)
table(valueNA)

# NA replacement
data3 <- data
data3$steps <- mapply(fillFunc, data3$steps, data3$interval)

# Plot with substituted elements
stepsTotal <- tapply(data3$steps, data3$date, FUN=sum)
qplot(stepsTotal, binwidth=1000, 
      xlab="Steps per day, total",
      ylab="Frequency")
mean(stepsTotal)
median(stepsTotal)

## Aplication of dayDef FUN
data3$date <- as.Date(data3$date)
data3$day <- sapply(data3$date, FUN=dayDef)

## Answer to: Are there differences in activity patterns between weekdays and weekends?
averagedData <- aggregate(steps ~ interval + day, data=data3, mean)
ggplot(averagedData, aes(interval, steps)) + 
        geom_area() + 
        facet_grid(day ~ .) + 
        xlab("5-minute interval") + 
        ylab("Number of steps")


