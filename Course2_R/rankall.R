rankall <- function(outcome, num = "best") {
    ## Read outcome data
    od <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## store possible state and outcome values
    availableStates <- unique(od[, 7])
    validOutcomes <- c("heart attack", "heart failure", "pneumonia")
    
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
    
    if (!is.na(as.numeric(num)) & num > sum(!is.na(ods$m))) {
        return(NA)
    }    
    
    ## generate order based on num value
    if (num == "worst") {
        ##result <- ods[ods$m == min(ods$m, na.rm = TRUE), ] [, 1]
        dec <- TRUE
    } else {
        dec <- FALSE
    }
    
    rankNum <- 1
    if (!is.na(as.numeric(num))) {    
        rankNum = num
    }

    spl <- split(1:nrow(ods), ods$s)
    df <- data.frame()
    for (i in seq_along(spl)) {    
        sd <- ods[spl[i][[1]],]
        o <- order(sd[, 3], sd[1], decreasing = dec)
        odm <- sd[o,]
        
        dff <- data.frame(hospital = odm[rankNum, 1], state = odm[1, 2])
        
        df <- rbind(df, dff)
        ##print("****")  
        ##print(df)  
        
        
    }    
    
    return(df)
}