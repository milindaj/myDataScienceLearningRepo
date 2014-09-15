## This file has 2 functions makeCacheMatrix() and cacheSolve()
## which are used to compute the inverse of matrix and cache the data.
## Cached data can be used later without computing the inverse again
## in case the original matrix has not changed

## this function takes a invertible matrix and returns a special 
## list object containing helper fucntions which can be used to 
## get and set matrix and its inverse value
makeCacheMatrix <- function(x = matrix()) {
    ## initialize the varible used to hold inverse of matrix to NULL
    invMatrix <- NULL

    
    ## checked if the data passed function is matrix
    ## If not exit the fucntion with a message
    if(class(x) != "matrix") {
        message("Invalid input value, matrix expected")
        return(NULL)
    }    
    
    ## function to store new matrix data
    setMatrix <- function(newMatrix) {
        x <<- newMatrix
        invMatrix <<- NULL ## as the matrix changing set inverse data to NULL
    }
    
    ## function to get matrix
    getMatrix <- function() x
    
    ## function to set inverse value of matrix. This fucntion will be
    ## used by cacheSolve() fucntion
    setInverse <- function(inverseMatrix) invMatrix <<- inverseMatrix
    
    ## function to get inverse value of matrix
    getInverse <- function() invMatrix

    ## special object with function list
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
  
}


## this function takes a special object created by  makeCacheMatrix() 
## function and returns the inverse of the matrix. If the inverse
## is already cached then the fuction retuns the cached data or else
## it computes inverse matrix, caches this data and also returns it
cacheSolve <- function(x, ...) {
    
    invMatrix <- x$getInverse()
    
    ## checked if invMatrix data is not NULL (which indicates it is cached previously)
    ## and return this value
    if(!is.null(invMatrix)) {
        message("getting cached data")
        return(invMatrix)
    }
    
    ## if cached data not present (i.e. when invMatrix is NULL)
    ## then compute inverse matrix, cache it and return inverse matrix
    data <- x$getMatrix()
    invMatrix <- solve(data, ...) ## solve used to compute inverse matrix
    
    ## call setInverse to cache inverse matrix data
    x$setInverse(invMatrix)
    
    ## return inverse matrix
    invMatrix
}
