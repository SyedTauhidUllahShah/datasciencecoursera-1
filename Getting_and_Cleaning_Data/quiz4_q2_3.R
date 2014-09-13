library(RCurl)

if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
destFile <- "./data/GDP.csv"
if(!file.exists(destFile)){
     download.file(fileUrl, destfile=destFile, method="curl")
}
GDPData <- read.csv(destFile, skip=5, header=FALSE,
                    blank.lines.skip=TRUE,
                    colClasses=c(NA,NA,"NULL",NA,NA,
                                 "NULL","NULL","NULL","NULL","NULL"))

numGDProws <- 236-5
GDPData <- GDPData[1:numGDProws,]
colnames(GDPData)[1] <- "Country"
colnames(GDPData)[2] <- "GDP.Rank"
colnames(GDPData)[3] <- "countryNames"
colnames(GDPData)[4] <- "GDP.Value"

GDPVals <- GDPData$GDP.Value

firstbadvalue <- which(GDPVals=="" | is.na(GDPVals) | GDPVals=="..")[1]
lastgoodvalue <- firstbadvalue-1
GDPVals <- GDPVals[1:lastgoodvalue]

GDPVals<-sapply(GDPVals,
                function(v) {as.numeric(gsub("\\,","",as.character(v)))})

print(mean(GDPVals))

countryNames <- GDPData$countryNames[1:lastgoodvalue]
countryNames<-sapply(countryNames,
                function(v) {gsub("\xe3","x",as.character(v))})

print(grep("^United",countryNames))