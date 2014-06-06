# As project #1 of course Exploratory Data Analysis" instructed,
# uses data set "Electric Power consumption" from UC Irving Machine 
# Learning Repo, make serveral plots to png files, this is the plot #3:

plot3 <- function() {
    # check if data file exists in local, if not, download; if 
    # exists, get it.
    dataSet <- Get.Dataset()
    
    # get the dataset cleaned based on the requirement of this plot.
    twoDdataset <- GetTidySet2(dataSet)
    
    # plot to png file
    png(filename = "plot3.png")
    plot(twoDdataset$date, twoDdataset$sub.m.value, 
         type = "n", ylab = "Energy sub metering", 
         xlab = "")
    with(subset(twoDdataset, e.sub.metering == "sub.metering.1"), 
         lines(date, sub.m.value, type = "l", col = "black"))
    with(subset(twoDdataset, e.sub.metering == "sub.metering.2"), 
         lines(date, sub.m.value, type = "l", col = "red"))
    with(subset(twoDdataset, e.sub.metering == "sub.metering.3"), 
         lines(date, sub.m.value, type = "l", col = "blue"))
    legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
           lty = 1, lwd = 1, col = c("black","red", "blue"))
    
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

GetTidySet2 <- function(x) {
    # turn x into a tidy dataset, make sub.metering.1,2,3 one variable
    library(reshape2)
    meltX <- melt(x, id = c("date"), measure.vars = 
            c("sub.metering.1", "sub.metering.2", "sub.metering.3"))
    meltX <- transform(meltX, date = strptime(date, "%d/%m/%Y %T"))
    colnames(meltX)[-1] <- c("e.sub.metering", "sub.m.value")
    return(meltX)
}