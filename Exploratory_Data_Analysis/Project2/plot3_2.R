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

# Make plot
png(filename = "plot3.png", width = 480*3, height = 480, units = "px")
p <- qplot(year, Emissions, data = NEI.Balt, facets = . ~ type) + geom_smooth(method="lm")
print(p)
dev.off()