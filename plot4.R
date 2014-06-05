# As project #1 of course Exploratory Data Analysis" instructed,
# uses data set "Electric Power consumption" from UC Irving Machine 
# Learning Repo, make serveral plots to png files, this is the plot #4:

plot4 <- function() {
    # check if data file exists in local, if not, download; if 
    # exists, get it.
    dataSet <- Get.Dataset()
    
    # get the dataset cleaned based on the requirement of this plot.
    tidySet1 <- GetTidySet(dataSet)
    tidySet2 <- GetTidySet(dataSet, toMelt = T)
    
    # plot to png file, this file will contain 4 plots in 2X2 positions
    png(filename = "plot4.png")
    par(mfrow = c(2,2))
    
    # plot on the upper left.
    with(tidySet1, plot(date, global.active.power, type = "l", 
                           ylab = "Global Active Power (kilowatts)", 
                            xlab = ""))
    
    # plot on the upper right.
    with(tidySet1, plot(date, voltage, type = "l", 
                        ylab = "Voltage", xlab = "datetime"))
    
    # plot on the lower left.
    with(tidySet2, plot(date, sub.m.value, type = "l", 
                        ylab = "Energy sub metering", xlab = ""))
    with(subset(tidySet2, e.sub.metering == "sub.metering.1"), 
         lines(date, sub.m.value, col = "black"))
    with(subset(tidySet2, e.sub.metering == "sub.metering.2"), 
         lines(date, sub.m.value, col = "red"))
    with(subset(tidySet2, e.sub.metering == "sub.metering.3"), 
         lines(date, sub.m.value, col = "blue"))
    legend("topright", bty = "n",
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
           lty = 1, lwd = 1, col = c("black","red", "blue"))
    
    # Plot on the lower right.
    with(tidySet1, plot(date, global.reactive.power, type = "l",
         ylab = "Global_reactive_power", xlab = "datetime"))
    
    dev.off()
}

Get.Dataset <- function() {
    # check if the specified data file exist in the particular
    # folder, if not, download from the specified url link. 
    file.path <- "./data/HPconsumption"
    if (!file.exists(file.path)) {
        if (!file.exists("./data")) {dir.create("./data")}
        dir.create("./data/HPconsumption")
        fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        download.file(fileUrl, destfile = 
                          "./data/HPconsumption.zip")
        unzip("./data/HPconsumption.zip", exdir =
                  "./data/HPconsumption")
        # since the file unzipped, the original zip file will be no
        # use, should be removed.
        file.remove("./data/HPconsumption.zip")
    }
    
    # read the data file to R dataframe, and make it clean as
    # requested.
    txtFile <- "./data/HPconsumption/household_power_consumption.txt"
    fullDataSet <- read.table(txtFile, header = T, sep = ";", quote = "",
                              na.strings = "?", colClasses = 
                                  c("character", "character", 
                                    rep("numeric", 7)))
    subsetData <- fullDataSet[fullDataSet$Date == "1/2/2007" | 
                                  fullDataSet$Date == "2/2/2007" |
                                  (fullDataSet$Date == "3/2/2007" & 
                                       fullDataSet$Time == "00:00:00"),]
    # remove fullDataSet to free the memory
    rm(fullDataSet)
    
    # rename the variables
    nameSet <- names(subsetData)
    nameSet <- gsub("_", "\\.", nameSet)
    nameSet <- tolower(nameSet)
    colnames(subsetData) <- nameSet
    
    # combine Date and Time columns.
    subsetData <- transform(subsetData, date = 
                                paste(date, time), time = NULL, row.names = NULL)
    
    return(subsetData)
}

GetTidySet3 <- function(x = data.frame()) {
    # turn x into a tidy dataset:
    # 1. melt sub.metering.1,2,3 to two variables e.sub.metering and 
    #    sub.m.value;
    # 2. subset dataset, only keep date, global.active.power, voltage,
    #    e.sub.metering, golbal.reactive.power variables;
    # 3. turn date column into real date time column using strptime.
    #
    library(reshape2)
    meltX <- melt(x, id = c("date", "global.active.power", "voltage",
                            "global.reactive.power"), measure.vars = 
                             c("sub.metering.1", "sub.metering.2", 
                               "sub.metering.3"), 
                  variable.name = "e.sub.metering",
                  value.name = "sub.m.value")
    meltX <- transform(meltX, date = strptime(date, "%d/%m/%Y %T"))
    return(meltX)
}

GetTidySet <- function(x = data.frame(), toMelt = F) {
    # turn x into a tidy dataset
    #
    if (!toMelt) {
        x <- transform(x, date = strptime(date, "%d/%m/%Y %T"))
        tidySet <- x[,c(1:5)]
        return(tidySet)
    } else {
        # turn x into a tidy dataset, make sub.metering.1,2,3 
        # one variable e.sub.metering.
        library(reshape2)
        meltX <- melt(x, id = c("date"), measure.vars = 
                          c("sub.metering.1", "sub.metering.2", "sub.metering.3"),
                      variable.name = "e.sub.metering",
                      value.name = "sub.m.value")
        meltX <- transform(meltX, date = strptime(date, "%d/%m/%Y %T"))
        return(meltX)
    }
    
}