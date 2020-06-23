pollutantmean <- function(directory, pollutant, id = 1:332) {
    result <- NA
    oldwd <- getwd()
    setwd(directory)    
    
    if (pollutant %in% c("nitrate", "sulfate")) {
        result <- 0
        count <- 0
        for (i in unlist(id)) {
            filename <- paste(sprintf("%03d", i), ".csv", sep="")
            data <- read.csv(filename)
            count <- count + sum(!is.na(data[,pollutant]))
            result <- result + sum(data[,pollutant], na.rm=TRUE)
        }
        result <- result / count
    }
        
    setwd(oldwd)
    result
}