rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    od <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## store possible state and outcome values
    availableStates <- unique(od[, 7])
    validOutcomes <- c("heart attack", "heart failure", "pneumonia")
    
    ## Check that state and outcome are valid
    if (! (state %in% availableStates)) {
        stop("invalid state")
        return(NULL)
    }   
    
    if (! (outcome %in% validOutcomes)) {
        stop("invalid outcome")
        return(NULL)
    }
    
    hCol <- 2
    stateCol <- 7
    
    ## assign outcome column based on the user enetered value
    if (outcome == "heart attack") {
        mCol = 11
    } else if (outcome == "heart failure") {
        mCol = 17
    } else {
        mCol = 23
    }    
    
    ## subset the data frame for easy operation
    ods <- od[, c(hCol,stateCol,mCol)]
    
    ## assign shorter column names
    names(ods) <- c("h", "s", "m")
    
    ## assign shorter column names
    ods$m <- as.numeric(ods$m)
        
    ## filter on selected state
    ods <- ods[ods$s == state,]
    
    if (!is.na(as.numeric(num)) & num > sum(!is.na(ods$m))) {
        return(NA)
    }    
    
    ## generate order based on num value
    if (num == "worst") {
        ##result <- ods[ods$m == min(ods$m, na.rm = TRUE), ] [, 1]
        o <- order (ods[, 3], ods[, 1], decreasing = TRUE)
    } else {
        o <- order (ods[, 3], ods[, 1])
    }
    
    if (!is.na(as.numeric(num))) {
        result <- ods[o, 1 ] [num]
    }else{
        result <- ods[o, 1 ] [1]
    }
    
    return(result)
}