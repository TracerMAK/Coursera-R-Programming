complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    oldwd <- getwd()
    setwd(directory)
    df <- data.frame("id" = NA, "nobs" = NA)
    ## 'id'is an integer vector indicating the monitor ID numbers
    ## to be used
    for (i in unlist(id)) {
        filename <- paste(sprintf("%03d", i), ".csv", sep="")
        data <- read.csv(filename)       
        nobs <- sum(complete.cases(data))
        df <- rbind(df, c(i, nobs))
    }
    ## Return a data frame of the form:
    ## id nobs
    ## 1	117
    ## 2	1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    setwd(oldwd)
    df <- df[-1,]
    row.names(df) <- seq(nrow(df))
    df
}