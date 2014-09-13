library(plyr)
unzip("repdata-data-activity.zip")
data <- read.csv("activity.csv")

data$date <- as.Date(data$date)
TotalSteps <-ddply(data, .(date), summarize, total_steps=sum(steps))