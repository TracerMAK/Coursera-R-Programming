rankall <- function(outcome, num = "best") {
    data <- read.csv("data/outcome-of-care-measures.csv",colClasses="character")
    outcomes <- c("HEART ATTACK"=11,"HEART FAILURE"=17,"PNEUMONIA"=23) 
    outcome <- toupper(outcome)
    if (num == "best") num <- 1

    if (!outcome %in% names(outcomes)) {
        stop("invalid outcome")
        return()
    }
    
    outcome_col <- outcomes[names(outcomes) == outcome]
    hospital_col <- which(names(data) == "Hospital.Name")
    state_col <- which(names(data) == "State")    
    
    ## Subset the columns, convert rates to numeric, remove missing values, and
    ## split into groups by state.
    rates <- data[,c(hospital_col, state_col, outcome_col)]
    rates[,3] <- suppressWarnings(as.numeric(rates[,3]))
    rates <- rates[complete.cases(rates),]     
    rates <- split(rates,rates$State)
    
    ## Loop through each group and find the requested rate rank. In case of ties
    ## order hospitals alphabetically to pick a result.
    result <- data.frame(hospital=character(), state=character())
    for (group in rates) {
        rank <- ifelse(num=="worst", dim(group)[1], num)
        group <- group[order(group[,3], group[,1]),]
        row <- data.frame(hospital=group[rank,1], state=unique(group$State),
                   row.names=unique(group$State))
        result <- rbind(result, row)
    }
    result
}
