best <- function(state, outcome) {
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
    
    rates <- data[which(data$State == state), c(hospital_col, outcome_col)] 
    best_outcome <- suppressWarnings(min(as.numeric(rates[,2]), na.rm=TRUE))
    hospitals <- rates[rates[,2] == format(best_outcome, nsmall=1),1]

    sort(hospitals)[1]
}