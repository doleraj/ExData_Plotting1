library(dplyr)

createPlot3 = function() {
    # Load the data.
    loadCache <<- makeLoadCache("household_power_consumption.txt")
    data <- loadCache$get()
    
    # Make the plot itself.
    png(file = "plot3.png", width=480, height=480)
    with(data, plot(Time, Sub_metering_1, type="l",
         ylab = "Energy sub metering", xlab=""))
    with(data, points(Time, type="l", Sub_metering_2, col="red"))
    with(data, points(Time, type="l", Sub_metering_3, col="blue"))
    legend("topright", col = c("black", "red", "blue"), lty=1, 
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
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