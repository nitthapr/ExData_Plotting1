## This R file code draw plot3
## **load function in part: Funtion of this file
## **before run part: main


############## Main ##################
## Load data between date 2007-02-01 to 2007-02-02.
## Read more about loadConsumption() at part: Function  of this file
dfConsOfInterest <- loadConsumption()

## Draw graph and write to output file
outputFile <- "plot3.png"
png(file = outputFile , width = 480, height = 480)

with(
        dfConsOfInterest, plot(
                dfConsOfInterest$DateTime , dfConsOfInterest$Sub_metering_1, 
                ylab = "Energy sub metering", xlab ="" , type = "n"
        )
)

with(
        dfConsOfInterest,
        points(
                dfConsOfInterest$DateTime , dfConsOfInterest$Sub_metering_1 ,
                type = "l", col = "black"
        )
)
with(
        dfConsOfInterest,
        points(
                dfConsOfInterest$DateTime , dfConsOfInterest$Sub_metering_2 ,
                type = "l", col = "red"
        )
)
with(
        dfConsOfInterest,
        points(
                dfConsOfInterest$DateTime , dfConsOfInterest$Sub_metering_3 ,
                type = "l", col = "blue"
        )
)

legend( "topright" , pch = 1, col = c( "black","red" , "blue" ) , 
        legend = c("sub_metering_1","sub_metering_2","sub_metering_3") )

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
