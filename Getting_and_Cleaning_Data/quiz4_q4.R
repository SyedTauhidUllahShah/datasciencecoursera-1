library(RCurl)

if(!file.exists("./data")){dir.create("./data")}
fileUrl1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
destFile1 <- "./data/GDP.csv"
fileUrl2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
destFile2 <- "./data/EduData.csv"
if(!file.exists(destFile1)){
     download.file(fileUrl1, destfile=destFile1, method="curl")
}
if(!file.exists(destFile2)){
     download.file(fileUrl2, destfile=destFile2, method="curl")
}
GDPData <- read.csv(destFile1, skip=5, header=FALSE,
                    blank.lines.skip=TRUE,
                    colClasses=c(NA,NA,"NULL",NA,NA,
                                 "NULL","NULL","NULL","NULL","NULL"))

numGDProws <- 236-5
GDPData <- GDPData[1:numGDProws,]
EduData <- read.csv(destFile2,header=TRUE)
colnames(GDPData)[1] <- "Country"
colnames(GDPData)[2] <- "GDP.Rank"
colnames(GDPData)[3] <- "Economy"
colnames(GDPData)[4] <- "GDP.Value"

mergedData = merge(GDPData, EduData, by.x="Country",
                  by.y="CountryCode",all=TRUE)

fiscaljuneitems <- grep("[Ff]iscal+ (.*) [Jj]une",mergedData$Special.Notes)
fiscaljune <- mergedData$Special.Notes[fiscaljuneitems]

print(fiscaljune)
print(length(fiscaljuneitems))