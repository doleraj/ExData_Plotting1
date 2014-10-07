library(dplyr)

createPlot1 = function() {
    # Load the data.
    loadCache <<- makeLoadCache("household_power_consumption.txt")
    data <- loadCache$get()
    
    # Make the plot itself.
    png(file = "plot1.png", width=480, height=480)
    hist(data$Global_active_power, col="red", main="Global Active Power", 
         xlab = "Global Active Power (kilowatts)")
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