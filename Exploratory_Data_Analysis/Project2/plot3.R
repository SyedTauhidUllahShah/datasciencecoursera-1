library(ggplot2)
library(plyr)
# Read in the data
NEI <- readRDS("summarySCC_PM25.rds")

# Get list of years
years <- unique(NEI$year)
num_years <- length(years)

# Subset the data for Baltimore only
NEI.Balt <- subset(NEI, fips == "24510")

# remove unneeded columns from the Baltimore data
NEI.Balt <- NEI.Balt[,4:6]

# Generate table with sums for each type
SummaryData <- ddply(NEI.Balt, .(type, year), summarize, total=sum(Emissions))
colnames(SummaryData)[3] <- "total_PM2.5"

# Make plot
png(filename = "plot3.png", width = 480*3, height = 480, units = "px")
p<- qplot(year, total_PM2.5, data = SummaryData, facets = . ~ type) + geom_smooth(method="lm")
print(p)
dev.off()