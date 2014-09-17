SW1 <- data.frame(c1 = c(1, NA, NA, NA, NA, 9, NA, 2, NA),
                 c2 = c(NA, 7, NA, NA, NA, 6, NA, NA, 4),
                 c3 = c(NA, NA, 3, 5, 1, NA, NA, NA, 8),
                 c4 = c(NA, NA, 7, 2, NA, NA, NA, NA, 5),
                 c5 = c(NA, 4, NA, NA, NA, 1, NA, 6, NA),
                 c6 = c(8, NA, NA, 9, NA, NA, 3, NA, NA),
                 c7 = c(2, 8, NA, NA, NA, 7, 9, NA, 1),
                 c8 = c(NA, NA, 9, NA, NA, NA, NA, NA, NA),
                 c9 = c(6, 5, NA, 3, NA, 4, NA, 8, NA)
)

SW2 <- data.frame(c1 = c(7, NA, 6, 4, NA, NA, 2, NA, NA),
                  c2 = c(NA, NA, NA, NA, 5, 8, 7, NA, NA),
                  c3 = c(NA, NA, NA, NA, 7, 2, NA, 9, 4),
                  c4 = c(NA, 1, NA, NA, 2, NA, 6, 7, NA),
                  c5 = c(NA, NA, 7, NA, 9, 3, 5, 2, NA),
                  c6 = c(NA, NA, 3, NA, NA, NA, NA, NA, 8),
                  c7 = c(2, 8, NA, 3, 4, NA, NA, NA, 7),
                  c8 = c(NA, NA, 4, NA, NA, 7, NA, NA, NA),
                  c9 = c(NA, NA, 9, NA, NA, NA, NA, NA, NA)
)

SWM <- data.frame(c1 = c(NA, NA, 8, NA, 9, NA, 6, NA, NA),
                  c2 = c(NA, 6, NA, NA, 3, NA, NA, 8, NA),
                  c3 = c(3, NA, NA, 8, 6, 4, NA, NA, 7),
                  c4 = c(NA, NA, 6, NA, NA, NA, 4, NA, NA),
                  c5 = c(5, 3, NA, NA, NA, NA, NA, 7, 2),
                  c6 = c(NA, NA, 7, NA, NA, NA, 1, NA, NA),
                  c7 = c(6, NA, NA, 2, NA, 1, NA, NA, 9),
                  c8 = c(NA, 2, NA, NA, 8, NA, NA, 4, 1),
                  c9 = c(NA, NA, 1, NA, 5, NA, 2, NA, NA)
)



solveSudoku <- function(sDF) {
    #solved <- solveSimpleSudoku(sDF)
    solved <- FALSE
    if (class(solved) == "logical") {
        print("Unable to solve me")
        solved <- solveComplexSudoku(sDF)
        return(solved)
    } else {
        return (solved)
    }
    
    
}

solveComplexSudoku <- function(df) {
    print("solveComplexSudoku called...")
    print(df)
    for(r in 1:9) {        
        for (c in 1:9) {
            print(cat("****** NEW INTERATIOn STARTED ",c, " ", r))
            
            cellValue <- df[[r,c]]
            
            if(is.na(cellValue)) {
                c1 <- df[,c]
                byCol <- (1:9)[-c1[!is.na(c1)]]
                
                r1 <- df[r,]
                byRow <- (1:9)[-r1[!is.na(r1)]]
                
                fam <- getCellFamily(df, r, c)
                
                if(sum(is.na(fam)) == 9) {
                    cellValue <- Reduce(intersect, list(byCol,byRow))                
                } else {
                    byFam <- (1:9)[-fam[!is.na(fam)]]
                    
                    cellValue <- Reduce(intersect, list(byCol,byRow,byFam))                                    
                }

            }
            ##print(cellValue)
            if (length(cellValue) == 0) {
                print("NULL situation. returning *******  ")
                return (NULL)
            }
            else if(length(cellValue) == 1){
                df[[r,c]] <- cellValue                
            } else {
                print(cat(" sequeing along the cell value  ", cellValue))
                solvedStatus <- FALSE
                for (i in seq_along(cellValue)){
                    df[r,c] = cellValue[[i]]
                    ##return(solveComplexSudoku(df))
                    tmpVal <- solveComplexSudoku(df)
                    
                    if(is.null(tmpVal)) {
                        next
                    } else {
                        
                        if(sum(is.na(SW2)) == 0) return(df)
                        
                        solvedStatus <- TRUE        
                        break
                    }
                        
                }
                
                if (solvedStatus == FALSE) return(NULL)
                
            #print(paste(r, c, sep= ","), paste(cellValue, sep= " "), sep=" ")
            }
        }
    }
    print("solveComplexSudoku returning...")
    return (df)
    
}

solveSimpleSudoku <- function(df){

    solveCount <- sum(is.na(df))
    solveCounterChanging <- TRUE
    previousSolveCount <- solveCount
    
    itr <- 0
    while (solveCount > 0) {
        df <- getCellOptions(df)
        solveCount <- sum(is.na(df))
        itr <- itr + 1
        print (paste("solve iteration = ", itr, " solve count = ", solveCount, " ", sep = " "))
        print(df)
        
        if(itr > 200) return (FALSE)
        
        if(solveCount == previousSolveCount) {
            print("NOT able to solve *********")
            return (FALSE)
        }
        
        previousSolveCount = solveCount
    }
    
    return (df)
}


getCellOptions <- function(df) {
    updatedDf <- data.frame()
    
    for(r in 1:9) {        
        for (c in 1:9) {
            
            cellValue <- df[[r,c]]
            
            if(is.na(cellValue)) {
                c1 <- df[,c]
                byCol <- (1:9)[-c1[!is.na(c1)]]
                
                r1 <- df[r,]
                byRow <- (1:9)[-r1[!is.na(r1)]]
                
                fam <- getCellFamily(df, r, c)
                byFam <- (1:9)[-fam[!is.na(fam)]]
                
                cellValue <- Reduce(intersect, list(byCol,byRow,byFam))                
            }
            
            if(length(cellValue) == 1){
                df[[r,c]] <- cellValue                
            }
        }
    }
    return (df)
}

getCellFamily <- function(df, r, c) {
    
    rStart <- floor((r-1)/3)*3 + 1
    rEnd <- floor((r-1)/3)*3 + 3
    cStart <- floor((c-1)/3)*3 + 1
    cEnd <- floor((c-1)/3)*3 + 3
    dff <- df[rStart:rEnd, cStart:cEnd]
    return(c(dff[[1,1]], dff[[1, 2]], dff[[1, 3]], dff[[2,1]], dff[[2,2]], dff[[2,3]], dff[[3,1]], dff[[3,2]], dff[[3,3]]))
}