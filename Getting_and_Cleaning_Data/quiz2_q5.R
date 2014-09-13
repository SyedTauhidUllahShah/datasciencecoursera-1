require("RCurl")

fileURL2 <-"https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"
fileURL <- sub('https','http',fileURL2)
con = url(fileURL)

dat = read.fwf(con,skip=4, widths=c(12, 7, 4, 9, 4, 9, 4, 9, 4))

total_col_four <- sum(dat$V4)
print(total_col_four)