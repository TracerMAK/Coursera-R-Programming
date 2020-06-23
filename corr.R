corr <- function(directory, threshold = 0) {
    oldwd <- getwd()
    setwd(directory)
    result <- numeric()

    for (filename in dir()) {
        data <- read.csv(filename)
        good <- complete.cases(data)
        if (sum(good) > threshold) {
            input <- data[good,c("sulfate", "nitrate")]
            result <- append(result, cor(input$sulfate, input$nitrate))
        }        
    }

    setwd(oldwd)
    result
}    