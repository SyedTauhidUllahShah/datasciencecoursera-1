library(ggplot2)
library(plyr)
# Read in the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Get list of years
years <- unique(NEI$year)
num_years <- length(years)

# remove unneeded columns from the data
NEIreduced <- NEI[,c(2, 4:6)]
SCCreduced <- SCC[,c(1, 4)]

# remove the NEI and SCC data sets from memory
rm(NEI)
rm(SCC)

# Merge by SCC number
mergeData <- merge(NEIreduced, SCCreduced, by = "SCC")

# remove the NEIreduced and SCCreduced datasets from memory
rm(NEIreduced)
rm(SCCreduced)

# subset mergeData to retain only coal EI.Sector
mergeData <- mergeData[grepl("[Cc]oal", mergeData$EI.Sector),]

# Generate table with sums for each type
SummaryData <- ddply(mergeData, .(type, year), summarize, total=sum(Emissions))
colnames(SummaryData)[3] <- "total_PM2.5"

# Make plot
png(filename = "plot4.png", width = 480, height = 480, units = "px")
p<- qplot(year, total_PM2.5, data = SummaryData, facets = . ~ type)
print(p)
dev.off()