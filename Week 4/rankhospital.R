rankhospital <- function(state, outcome, num = "best") {
    data <- read.csv("data/outcome-of-care-measures.csv",colClasses="character")
    states <- unique(data[,which(names(data) == "State")])
    outcomes <- c("HEART ATTACK"=11,"HEART FAILURE"=17,"PNEUMONIA"=23) 
    state <- toupper(state)
    outcome <- toupper(outcome)
    
    if (!state %in% states) {
        stop("invalid state")
        return()
    }
    if (!outcome %in% names(outcomes)) {
        stop("invalid outcome")
        return()
    }
    
    outcome_col <- outcomes[names(outcomes) == outcome]
    hospital_col <- which(names(data) == "Hospital.Name")
    
    ## Get hospital and rate by state and remove missing values and then order
    ## by increasing rate with a second order by hospital name in case of ties
    rates <- data[which(data$State == state), c(hospital_col, outcome_col)]
    rates[,2] <- suppressWarnings(as.numeric(rates[,2]))
    rates <- rates[complete.cases(rates),]    
    rates <- rates[order(rates[,2], rates[,1]),]

    if (num == "best") num <- 1
    if (num == "worst") num <- dim(rates)[1]
    rates[num,1]
}