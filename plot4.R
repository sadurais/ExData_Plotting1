library(data.table)


read_power_data_efficient <- function() {
    # Lets download the zipFile and extract our inputFile off of it
    inputFile <- "household_power_consumption.txt"
    if (!file.exists(inputFile)) {
        zipFile <- "household_power_consumption.zip"
        fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        tryCatch({
            message("Downloading a huge zip file. Please wait....")
            download.file(fileUrl, destfile=zipFile, method="curl", quiet=TRUE)
        }, error = function(cond) {
            stop(paste("Error dowloading zipFile: ", zipFile,
                       ". Reason: ", cond))
        })

        tryCatch({
            unzip(zipFile, exdir=".")  # Unzip to current working dir
        }, error = function(cond) {
            stop(paste("Error extracting zipFile: ", zipFile,
                       ". Reason: ", cond))
        })
    }

    #
    # We know the following about the input file already.
    #   * It is considerably big (2M observations and 9 variables) but we
    #       ONLY NEED 2880 observations of date-range 2007FEB01-2007FEB02
    #       that are of interest to us.
    #   * 2007FEB01/02 data begins at line 66638
    #   * We know the variable-names (column-names) and their classes
    # So, lets read this file efficiently (incredibly faster compared
    # to read.csv or read.table) using fread.
    #
    varNames <- c("Date","Time", "Global_active_power",
                  "Global_reactive_power", "Voltage",
                  "Global_intensity", "Sub_metering_1",
                  "Sub_metering_2", "Sub_metering_3")
    varClasses <- c(rep("character", 2), rep("numeric", 7))
    df <- fread(inputFile, sep=";", header=FALSE, colClasses = varClasses,
                skip=66638-1, nrows=2880)
    setnames(df, varNames)

    # Time and Date are separate columns and are 'character' classes
    # so far. Join them and convert them to POSIXct time class. Also
    # assume times are in UTC
    df$DateTime <- as.POSIXct(paste(df$Date, df$Time), tz="UTC",
                              format="%d/%m/%Y %H:%M:%S")

    df
}


do_plot3 <- function(needLegendBorder="") {
    # Draw a blank graph with 'DateTime' in X-axis
    plot(x=df$DateTime, y=df$Sub_metering_1, type="n",
         ylab="Energy sub metering", xlab="")

    # Add the legend text to the topright corner
    legend("topright",
           legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
           col=c("black", "red", "blue"), lwd=2, cex=1.2, y.intersp=1.4,
           lty=c(1,1,1), bty=needLegendBorder)

    # Add 3 sub-metering values to Y-axis one-by-one
    lines(x=df$DateTime, y=df$Sub_metering_1, type="l", col="black")
    lines(x=df$DateTime, y=df$Sub_metering_2, type="l", col="red")
    lines(x=df$DateTime, y=df$Sub_metering_3, type="l", col="blue")
}


plot4 <- function(toFile=TRUE) {
    df <- read_power_data_efficient()

    if (toFile) {
        png(file="plot4.png", width=480, height=480, units="px")
    }

    par(mfcol=c(2,2))    # 4 plots in 2x2 arrangement
    par(mar=c(5,4,2,2))  # Make sure enought margin to display labels

    plot(df$DateTime, df$Global_active_power, type="l",
         ylab="Global Active Power", xlab="")

    do_plot3(needLegendBorder="n");

    # Draw a line-graph with 'DateTime' in X-axis
    # and 'Voltage' in Y-axis
    plot(df$DateTime, df$Voltage, type="l",
         ylab="Voltage", xlab="datetime")

    # Draw a line-graph with 'DateTime' in X-axis
    # and 'Global_reactive_power' in Y-axis
    plot(df$DateTime, df$Global_reactive_power, type="l",
         ylab="Global_reactive_power", xlab="datetime")

    #dev.copy(png, file = "plot4.png")  # Store the image to file
    if (toFile) dev.off()
}


plot4()   # Plot to file 'plot4.png'


