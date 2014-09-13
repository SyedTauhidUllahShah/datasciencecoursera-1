library(RCurl)
library(jpeg)

if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg "
destFile <- "./data/instructor.jpg"
if(!file.exists(destFile)){
     download.file(fileUrl, destfile=destFile, method="curl")
}
image <- readJPEG(destFile, native=TRUE)
quantiles <- quantile(image, probs = c(0.30, 0.80))
print(quantiles)