complete <- function(directory, id = 1:332) {
    oldwd <- getwd()
    setwd(directory)
    df <- data.frame("id" = NA, "nobs" = NA)

    for (i in unlist(id)) {
        filename <- paste(sprintf("%03d", i), ".csv", sep="")
        data <- read.csv(filename)       
        nobs <- sum(complete.cases(data))
        df <- rbind(df, c(i, nobs))
    }
    
    setwd(oldwd)
    df <- df[-1,]
    row.names(df) <- seq(nrow(df))
    df
}