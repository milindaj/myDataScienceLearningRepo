best <- function(state, outcome) {
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
    
    if (outcome == "heart attack") {
        mCol = 11
    } else if (outcome == "heart failure") {
        mCol = 17
    } else {
        mCol = 23
    }    
    
    ods <- od[, c(2,7,mCol)]
    
    ##ods <- od[, c(2,7,11)]
    
    names(ods) <- c("h", "s", "m")
    
    ods$m <- as.numeric(ods$m)
    ods$m[is.na(ods$m)] <- 9999
        
    ods <- ods[ods$s == state,]
    
    ##odstx <- ods[ods$s == state,]
    
    ##result <- with(odstx, h[m== min(m[s==state])])
    
    ##result <- odstx[odstx$m == min(odstx$m[odstx$s==state], na.rm = TRUE) & odstx$s==state & !is.na(odstx$m), ] [, 1]
    
    ##result <- ods[ods$m == min(ods$m[ods$s==state], na.rm = TRUE) & ods$s==state & !is.na(ods$m), ] [, 1]
    
    result <- ods[ods$m == min(ods$m, na.rm = TRUE) & !is.na(ods$m), ] [, 1]
    
    #result <- with(ods, h[m== min(m, na.rm = T)]) [, 1]
    
    result <- result[sort.list(result)] [1]
    
    #result <- odstx[odstx$m == 14.6 & odstx$s=="TX" & !is.na(odstx$m), ]
    
    #result <- with(odstx, h[m==min(m[s==state]) & s==state])
    
    #result <- with(odstx, h[m== min(m, na.rm = T)])
    
    #with(odstx, h[m== min(odstx$m, na.rm = T)])
    
    #odstx[odstx$m == 12]
    
    
    
    ##odx <- sapply(split(1:nrow(ods), ods$s), function(x) {
    ##    x[which.max(ods[x,"m"])]
##    })    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    return(result)
}