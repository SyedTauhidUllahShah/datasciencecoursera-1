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

hasGDPRank <- mergedData$GDP.Rank!="" & !is.na(mergedData$GDP.Rank)
mergedData <- mergedData[hasGDPRank,]

mergedData$GDP.Rank<-as.numeric(as.character(mergedData$GDP.Rank))

mergedData = mergedData[order(mergedData$GDP.Rank, decreasing=TRUE),]

print(sum(hasGDPRank))

print(mergedData$Long.Name[[13]])

minData <- data.frame(Country = mergedData$Country, 
                      GDP.Rank = mergedData$GDP.Rank,
                      Income.Group = mergedData$Income.Group)

minDataHighIncomeOECD <- 
     minData[minData$Income.Group=="High income: OECD",]
minDataHighIncomeNonOECD <- 
     minData[minData$Income.Group=="High income: nonOECD",]

print(mean(minDataHighIncomeOECD$GDP.Rank,na.rm=TRUE))
print(mean(minDataHighIncomeNonOECD$GDP.Rank,na.rm=TRUE))

GDP.Rank.Quantile <- cut(minData$GDP.Rank, 5)
quantileDF <- data.frame(GDP.Rank = minData$GDP.Rank,
                   Income.Group = minData$Income.Group,
                   Bin = GDP.Rank.Quantile)

print(table(minData$Income.Group,GDP.Rank.Quantile))

# quantileGDPRank <- quantile(minData$GDP.Rank,
#                             probs=c(0,0.2,0.4,0.6,0.8,1),
#                             na.rm=TRUE)