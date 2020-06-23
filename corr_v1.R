corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    oldwd <- getwd()
    setwd(directory)
    result <- numeric()
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all variables)
    ## required to compute the correlation between nitrate and sulfate;
    ## the default is 0
    for (filename in dir()) {
        data <- read.csv(filename)
        good <- complete.cases(data)
        if (sum(good) > threshold) {
            input <- data[good,c("sulfate", "nitrate")]
            result <- append(result, cor(input$sulfate, input$nitrate))
        }        
    }
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    
    setwd(oldwd)
    result
}    