library(ggplot2)
library(plyr)
# Read in the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Get list of years
years <- unique(NEI$year)
num_years <- length(years)

# Subset the data for Baltimore city and Los Angeles County
NEI <- subset(NEI, fips == "24510" | fips =="06037")

# remove unneeded columns from the data
NEIreduced <- NEI[,c(1:2, 4:6)]
SCCreduced <- SCC[,c(1, 4)]

# Merge by SCC number
mergeData <- merge(NEIreduced, SCCreduced, by = "SCC")

# subset mergeData to retain only coal EI.Sector
mergeData <- mergeData[grepl("[Vv]ehicle", mergeData$EI.Sector),]

# Generate table with sums for each type
SummaryData <- ddply(mergeData, .(type, year, fips), summarize, total=sum(Emissions))
colnames(SummaryData)[4] <- "total_PM2.5"

# Make plot
png(filename = "plot6.png", width = 480, height = 480, units = "px")
p <- qplot(year, total_PM2.5, data = SummaryData, col = fips, 
           main = "Motor Vehicle Emissions in Baltimore City Vs. Los Angeles County")
print(p)
dev.off()