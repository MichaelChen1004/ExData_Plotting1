# As project #1 of course Exploratory Data Analysis" instructed,
# uses data set "Electric Power consumption" from UC Irving Machine 
# Learning Repo, make serveral plots to png files, this is the plot #1:

plot1 <- function() {
    # check if data file exists in local, if not, download it; if 
    # exists, then get it.
    dataSet <- Get.Dataset()
    
    # clean the dataset according to the requirement of this plot.
    twoDdataset <- GetTidySet1(dataSet)
    
    # plot a histogram, and save it to plot1.png
    hist(twoDdataset$global.active.power, main = 
             "Global Active Power", xlab = 
             "Global Active Power (kilowatts)", col = "red")
    # save to png file.
    dev.copy(png, file = "plot1.png")
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
    
    # combine Date and Time columns;
    subsetData <- transform(subsetData, date = paste(date, time), 
                            time = NULL, row.names = NULL)
    
    return(subsetData)
}

GetTidySet1 <- function(x = data.frame()) {
    # turn x into a tidy dataset, keep only date and 
    # global.active.power variables.
    #
    x <- transform(x, date = strptime(date, "%d/%m/%Y %T"))
    tidySet <- x[,c(1,2)]
    return(tidySet)
}