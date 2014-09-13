library(sqldf)

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
fileURL2 <- sub('https','http',fileURL)

acs <- read.csv(fileURL2)

#sqldf("select pwgtp1 from acs where AGEP < 50")

#sqldf("select distinct AGEP from acs")