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
Hist$date <- format(Hist$date, format="%m - %d")
h <- ggplot(Hist, aes(factor(date), Total.Steps))
h + geom_bar(stat="identity", color = "black", fill = "purple") + 
        theme(axis.text.x = element_text(angle = 90)) +
        xlab("Month and Day in 2012") + ylab("Total Steps") + 
        ggtitle("Total Number of Steps Taken Each Day") +
        theme(plot.title = element_text(color = "black", size= 11),
              axis.title.x = element_text(color = "darkblue"),
              axis.title.y = element_text(color = "darkblue"))
##3. Mean and Median Number of Steps Each Day
mandm <- summarise_each(byDay, 
                        funs(mean(.,na.rm = TRUE),median(.,na.rm = TRUE)),
                        steps)
mandm$date <- format(mandm$date, format="%m - %d")
p1 <- ggplot(mandm, aes(factor(date), mean)) + 
        geom_bar(stat="identity", color = "black", fill = "purple") +
        xlab("Month and Day in 2012") +
        theme(axis.text.x = element_text(angle = 90))
p2 <- ggplot(mandm, aes(factor(date), median)) +
        geom_boxplot() +
        xlab("Month and Day in 2012") +
        theme(axis.text.x = element_text(angle = 90))
p3 <- ggarrange(p1, p2, nrow = 2)
annotate_figure(p3, top = text_grob("Mean and Median Number of Steps Taken Each Day"))
##4. Time series plot of the average number of steps taken
