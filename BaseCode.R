##Base Code
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggpubr)
if(!file.exists("./data")){dir.create("./data")}
GeneralFiles <- list.files("./data", full.names = TRUE)
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = "./data/AMD.zip", method = "curl")
unzip("./data/AMD.zip", exdir = "./data" )
fn <- "./data/AMD.zip"
if (file.exists(fn))    file.remove(fn)
##1. Reading Files Code
AMD <- read.csv("./data/activity.csv")
AMD$date <- ymd(AMD$date)
##2. Histogram
byDay <- group_by(AMD, date)
Hist <- summarize(byDay, sum(steps, na.rm = TRUE))
names(Hist)[2] <- "Total.Steps"
h <- ggplot(Hist, aes(date, Total.Steps))
h + geom_bar(stat="identity", color = "black", fill = "purple") + 
        theme(axis.text.x = element_text(angle = 90)) +
        xlab("Month and Day in 2012") + ylab("Total Steps") + 
        ggtitle("Total Number of Steps Taken Each Day") +
        theme(plot.title = element_text(color = "black", size= 11),
              axis.title.x = element_text(color = "darkblue"),
              axis.title.y = element_text(color = "darkblue"))
##3. Mean and Median Number of Steps Each Day
TotalStepsMean <- mean(AMD$steps, na.rm = TRUE)
TotalStepsMedian <- median(AMD$steps, na.rm = TRUE)
mean(AMD$steps>0, na.rm = T) ##Porcentage of steps > 0
##4. Time series plot of the average number of steps taken
byInt <- group_by(AMD, interval)
mandm <- summarise_each(byInt, 
                        funs(mean(.,na.rm = TRUE),median(.,na.rm = TRUE)),
                        steps)
ggplot(mandm, aes(interval, mean)) + 
        geom_line(color="purple") +
        xlab("Time Interval") + ylab("Average Number of Steps")
##5. The 5-minute interval that, on average, contains the maximum number of steps
MaxStepI <- top_n(mandm, 1, mean)
MaxStepI
##6.  imputing missing data.
sum(is.na(AMD))
summary(AMD)
AMD2 <- AMD
tableMeans <- summarize(byDay, mean(steps, na.rm = TRUE))
names(tableMeans)[2] <- "Mean"
tableMeans[which(tableMeans$Mean == "NaN"),2] <- 0
for (i in 1:nrow(AMD2)) {
        if (is.na(AMD2[i,1])){
                AMD2[i,1] <- subset(tableMeans, date == AMD[i,2])[1,2]
        }
        
}
##7. Histogram of the total number of steps taken each day 
##after missing values are imputed
byDay2 <- group_by(AMD2, date)
Hist2 <- summarize(byDay2, sum(steps, na.rm = TRUE))
names(Hist2)[2] <- "Total.Steps"
h2 <- ggplot(Hist2, aes(date, Total.Steps))
h2 + geom_bar(stat="identity", color = "black", fill = "purple") + 
        xlab("Month and Day in 2012") + ylab("Total Steps") + 
        ggtitle("Total Number of Steps Taken Each Day") +
        theme(plot.title = element_text(color = "black", size= 11),
              axis.title.x = element_text(color = "darkblue"),
              axis.title.y = element_text(color = "darkblue"))
##8. 
weekends <- c("Saturday", "Sunday")
weekdays <- c("Friday", "Monday", "Thursday", "Tuesday", "Wednesday")
WEndsData <- subset(AMD2, weekdays(byDay$date) %in% weekends) %>% group_by(interval)
WDaysData <- subset(AMD2, weekdays(byDay$date) %in% weekdays) %>% group_by(interval)
WEData <- summarise(WEndsData, mean(steps, na.rm = T))
WDData <- summarise(WDaysData, mean(steps, na.rm = T))
names(WEData)[2] <- "Steps"; names(WDData)[2] <- "Steps"
we <- ggplot(WEData, aes(interval, Steps)) +
        geom_line(color="purple") +
        ggtitle("Weekends")

   
wd <- ggplot(WDData, aes(interval, Steps)) +
        geom_line(color="purple") +
        ggtitle("Weekdays")  

ggarrange(we,wd,nrow = 2)
