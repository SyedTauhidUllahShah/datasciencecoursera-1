library(plyr)
# Read in the data
NEI <- readRDS("summarySCC_PM25.rds")

# Get list of years
years <- unique(NEI$year)
num_years <- length(years)

# remove unneeded columns from the data
NEI <- NEI[,4:6]

# Generate table with sums for each type
SummaryData <- ddply(NEI, .(year), summarize, total=sum(Emissions))
colnames(SummaryData)[2] <- "total_PM2.5"

# find overall range for the y-axis
rng <- range(SummaryData$total_PM2.5)

# Set up tick marks for x and y axes
xticks = years
yticks = c(rng[1], mean(rng), rng[2])

#make plots
png(filename = "plot1.png", width = 480, height = 480, units = "px",
    bg = "white")
marker_cols = seq(1, length(years), by = 1)
plot(SummaryData$year, SummaryData$total_PM2.5 , xlab = "year",
     ylab = expression("Total PM"[2.5]),
     main = bquote("Total" ~ PM[2.5] ~ "Emissions Vs. Year"),
     axes = FALSE, pch = 20)
axis(side = 1, at = xticks, lab=xticks)
axis(side = 2, at = yticks, labels = sprintf("%.0f",yticks))
box()
dev.off()