# Read in the raw data with smaller subset close to dates of interest
# 2007-02-01 through 2007-02-02
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

# plot setup
png(filename = "plot4.png", width = 480, height = 480, units = "px",
    bg = "white")
par(mfrow = c(2,2))

# plot in upper left corner
with(data, plot(Global_active_power, col="black", type ="l",
                ylab = "Global Active Power",
                xlab ="", axes=FALSE))
# There are 2880 rows in the data set, so set the tick marks to be evenly spaced
# in that interval.  Also, the first day is Thu, so set the last tick mark
# equal to Sat
tick_pos = c(1, nrow(data)/2+1, nrow(data))
xlabels = c(data$dayofweek[tick_pos[1]], data$dayofweek[tick_pos[2]], "Sat")
axis(side = 1, at = tick_pos, lab = xlabels)
axis(side = 2)
box()

# plot in upper right corner
with(data, plot(Voltage, col="black", type ="l",
                ylab = "Voltage",
                xlab ="datetime", axes=FALSE))
axis(side = 1, at = tick_pos, lab = xlabels)
axis(side = 2)
box()


# plot in lower left corner
colors <- c("black","red","blue")
y_plot_data <- c("Sub_metering_1","Sub_metering_2","Sub_metering_3")
with(data, plot(Sub_metering_1, col=colors[1], type ="l",
                ylab = "Energy sub metering",
                xlab ="", axes=FALSE))
axis(side = 1, at = tick_pos, lab = xlabels)
axis(side = 2)
box()
     # Add the lines for Sub_metering_2 and 3
with(data, lines(Sub_metering_2, type="l", col=colors[2]) )
with(data, lines(Sub_metering_3, type="l", col=colors[3]) )
     # add legend
legend("topright", y_plot_data, col = colors, lty=c(1,1,1), bty="n")

# plot in lower right corner
with(data, plot(Global_reactive_power, col="black", type ="l",
                xlab ="datetime", axes=FALSE))
axis(side = 1, at = tick_pos, lab = xlabels)
axis(side = 2)
box()
dev.off()