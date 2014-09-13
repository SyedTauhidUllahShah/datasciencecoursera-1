library(RCurl)

if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
destFile <- "./data/housingIdaho.csv"
if(!file.exists(destFile)){
     download.file(fileUrl, destfile=destFile, method="curl")
}
housingData <- read.csv(destFile)
agricultureLogical <- (housingData$ACR==3 & housingData$AGS==6)
print(which(agricultureLogical))