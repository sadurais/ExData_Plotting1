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


plot1 <- function(toFile = TRUE) {
    df <- read_power_data_efficient()

    if (toFile) {
        png(file="plot1.png", width=480, height=480, units="px")
    }

    par(mfrow=c(1,1))    # Make sure only 1 plot in entire canvas
    par(mar=c(4,4,4,2))  # Make sure enought margin to display labels

    # Draw a histogram of Gloabl_active_power
    hist(df$Global_active_power, col="red", main = "Global Active Power",
         xlab = "Global Active Power (kilowatts)")

    # Store the image to file
    #png(filename = "Rplot%03d.png", width = 480, height = 480, units = "px",
    #    pointsize = 12, bg = "white",  res = NA, ...,
    #    type = c("cairo", "cairo-png", "Xlib", "quartz"), antialias)
    #dev.copy(device=png(width=480, height=480,
    #                    units="px"), file = "plot1.png")

    if (toFile) dev.off()
}


plot1(); # Plot to file 'plot1.png'

