# Read in the raw data with smaller subset close to dates of interest
# 2007-02-01 through 2007-02-02
#data <- read.table(pipe('grep "^[1-2]/2/2007" "household_power_consumption.txt"'),header=FALSE, sep=';')
data <- read.csv2("./household_power_consumption.txt", nrows = 8000,
                  header = TRUE, stringsAsFactors = FALSE, skip=65000)

# Set names of columns
colnames(data) <- c("Date","Time","Global_active_power",
                    "Global_reactive_power","Voltage","Global_intensity",
                    "Sub_metering_1","Sub_metering_2","Sub_metering_3")

# Combine the Date and Time columns into new column
data$datetime <- paste(data$Date, data$Time, sep=" ")
data$datetime <- strptime(data$datetime, format = "%d/%m/%Y %T")

# Convert columns to appropriate formats
data$Global_active_power <- as.numeric(data$Global_active_power)
data$Global_reactive_power <- as.numeric(data$Global_reactive_power)
data$Voltage <- as.numeric(data$Voltage)
data$Global_intensity <- as.numeric(data$Global_intensity)
data$Sub_metering_1 <- as.numeric(data$Sub_metering_1)
data$Sub_metering_2 <- as.numeric(data$Sub_metering_2)
data$Sub_metering_3 <- as.numeric(data$Sub_metering_3)

# Subset the data for dates only in range of interest
data$day <- data$datetime$mday          #day of month
data$month <- data$datetime$mon+1       #month of year (zer-indexed)
data$year <- data$datetime$year+1900    #years since 1900
data$dayofweek <- weekdays.POSIXt(data$datetime, abbreviate=TRUE)
data <- data[data$year == 2007 & data$month==2 & data$day>=1 & data$day<=2,]

# plot histogram
png(filename = "plot1.png", width = 480, height = 480, units = "px",
    bg = "white")
hist(data$Global_active_power, col="red", breaks=12,
     main = "Global Active Power", xlab = "Global Active Power (kilowatts)",
     ylab = "Frequency")
dev.off()