## This R file code draw plot1
## **load function in part: Funtion of this file
## **before run part: main


############## Main ##################
## Load data between date 2007-02-01 to 2007-02-02.
## Read more about loadConsumption() at part: Function  of this file
dfConsOfInterest <- loadConsumption()

## Draw graph and write to output file
outputFile <- "plot1.png"
png( file = outputFile , width = 480, height = 480)
hist(
        
        dfConsOfInterest$Global_active_power , col = "red", 
        xlab = "Global Active Power(kilowatts)", main = "Global Active Power"
)
dev.off()



############## Function ##############
#### load Consumption is function for loading household_power_consumption.txt
#### only date between date 2007-02-01 to 2007-02-02
loadConsumption   <-
        function()  {
                fileName <- "household_power_consumption.txt"
                ## 1.) Load all data and store in dfCons
                dfCons <-
                        read.table(
                                fileName, nrows = 30, sep = ";" , header = TRUE, na.strings = "?"
                        )
                classes <- sapply(dfCons, class)
                dfCons <-         read.table(
                        fileName, colClasses = classes , sep = ";" , header = TRUE, na.strings = "?"
                )
                
                ## 2.) subset only data from date 2007-02-01 to 2007-02-02 and store in dfConsOfIterested
                dfCons$DateAsDate <-
                        as.Date(dfCons$Date , format = "%d/%m/%Y")
                startDate = as.Date("2007-02-01")
                endDate = as.Date("2007-02-02")
                
                dfConsOfInterest <-
                        dfCons[which (dfCons$DateAsDate >= startDate  &
                                              dfCons$DateAsDate <= endDate),]
                dfConsOfInterest <- dfConsOfInterest[c(1:9)]
                
                ## 3.) Add Column DateTime
                dfConsOfInterest$DateTime <-
                        as.POSIXct(paste(dfConsOfInterest$Date , dfConsOfInterest$Time), format = "%d/%m/%Y %H:%M:%S")
                dfConsOfInterest
                
        }



