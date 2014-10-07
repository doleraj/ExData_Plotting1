library(dplyr)

createPlot4 = function() {
    # Load the data.
    loadCache <<- makeLoadCache("household_power_consumption.txt")
    data <- loadCache$get()
    
    # Make the plot itself.
    png(file = "plot4.png", width=480, height=480)
    par(mfrow=c(2, 2))
    with(data, {
        plot(Time, Global_active_power, type="l", ylab = "Global Active Power", xlab="")
        plot(Time, Voltage, type="l", ylab = "Voltage", xlab="datetime")
        plot(Time, Sub_metering_1, type="l", ylab = "Energy sub metering", xlab="")
        points(Time, type="l", Sub_metering_2, col="red")
        points(Time, type="l", Sub_metering_3, col="blue")
        legend("topright", col = c("black", "red", "blue"), lty=1, bty="n",
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
       plot(Time, Global_reactive_power, type="l", xlab="datetime")
    })
    dev.off()
}

# Cache the data so re-running the method doesn't take as long.
makeLoadCache <- function(filename) {
    data <- NULL
    get <- function() {
        if (!is.null(data)) {
            return(data)
        } else {
            data <<- 
                tbl_df(read.csv(filename, sep = ";", header=TRUE, na.strings = "?")) %>%
                mutate(Date = as.Date(Date, format="%d/%m/%Y")) %>%
                mutate(Time = as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S")) %>%
                filter(Date >= as.Date("2007-02-01"), Date <= as.Date("2007-02-02"))
            data
        }
    }
    list(get = get)
}