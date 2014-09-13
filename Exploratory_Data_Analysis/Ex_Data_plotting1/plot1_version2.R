data <- read.table(pipe('grep "^[1-2]/2/2007" "household_power_consumption.txt"'),header=FALSE, sep=';')
png(filename='plot1_version2.png', width=480, height=480)
hist(data$V3, col='red', main='Global Active Power', xlab='Global Active Power (kilowatts)',bg = "transparent")
dev.off()