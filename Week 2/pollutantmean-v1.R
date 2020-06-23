## Calculating each file mean separately and then taking the mean of all the
## separate values results in rounding errors. The resulting mean does not
## match the demo test mean.

pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length l indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values) without any rounding.
    
    result <- NA
    oldwd <- getwd()
    setwd(directory)    
    
    if (pollutant %in% c("nitrate", "sulfate")) {
        result <- c()
        for (i in unlist(id)) {
            ## Open each <i>.csv file, call mean(table$pollutant, na.rm=TRUE)
            ## to calculate the mean of the column. Append the value to 'result'
            ## vector.
            filename <- paste(sprintf("%03d", i), ".csv", sep="")
            data <- read.csv(filename)
            result <- append(result, mean(data[,pollutant], na.rm=TRUE))
        }
        ## Call result <- mean(result) to calculate the final mean.
        result <- mean(result)
    }
        
    setwd(oldwd)
    result
}