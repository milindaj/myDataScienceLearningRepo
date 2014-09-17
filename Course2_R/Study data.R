
x <- c("a", "blist", "c", "d")

m <- matrix(1:10, 2, 5)

df2 <- data.frame(foo=1:4, bar=c("a", "b", "c", "d"))

xna <- c("a", "b", "c", NA, NA, "d", NaN, "e")

x <- c(1, 2, NA, 4, NA, 6)
y <- c("a", "b", NA, "d", NA, NA, "g")


x <- 1:9; names(x) <- x
# Multiplication & Power Tables
x %o% x
y <- 2:8; names(y) <- paste(y,":", sep = "")
outer(y, x, "^")

outer(month.abb, 1999:2003, FUN = "paste")

## three way multiplication table:
x %o% x %o% y[1:3]


makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}






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


Assignment 3

ods <- od[, c(2,7,11)]
ods$m <- as.numeric(ods$m)
ods$m[is.na(ods$m)] <- 0

with(ods, h[m== max(m[s=="TX"])])

odx <- sapply(split(1:nrow(ods), ods$s), function(x) {
    x[which.max(ods[x,"m"])]
})

odx <- sapply(split(1:nrow(ods), ods$s), function(x) {
    x[which.max(ods[x,"m"])]
})

odx <- sapply(split(1:nrow(ods), ods$s), function(x) {
    x[which.max(ods[x,"m"])]
})

odxx <- sapply(split(odx, names(odx)), function(x) {
    ods[ods$s == x[1], x[2]]
})

split(odx, names(odx)) [1]

for (i in seq_along(availableStates)) {
    print(ods[odx[[availableStates[i]]], 1])
}

df <- data.frame()

sapply(split(1:nrow(ods), ods$s), function(x) {
    
    ##o <- order (ods[, 2], ods[, 3], ods[, 1])
    ##print(x)
    sd <- ods[x[[1]],]
    o <- order(sd[, 3], sd[1])
    odm <- sd[o,]
    
    dff <- data.frame(hospital = odm[1, 1], state = odm[1, 2])
    df <- rbind(df, dff)
    
})

spl <- split(1:nrow(ods), ods$s)
df <- data.frame()
sapply(spl, function(x) {
  
  sd <- ods[x[[1]],]
  o <- order(sd[, 3], sd[1])
  odm <- sd[o,]
  
  dff <- data.frame(hospital = odm[1, 1], state = odm[1, 2])
  
  df <- rbind(df, dff)
  print("****")  
  print(df)  
  return (df)
  
})

spl <- split(1:nrow(ods), ods$s)
df <- data.frame()
for (i in seq_along(spl)) {    
    sd <- ods[spl[i][[1]],]
    o <- order(sd[, 3], sd[1], decreasing = TRUE)
    odm <- sd[o,]
    
    dff <- data.frame(hospital = odm[1, 1], state = odm[1, 2])
    
    df <- rbind(df, dff)
    ##print("****")  
    ##print(df)  
    
    
}

for (i in seq_along(spl)) {    
    print(ods[spl[i],])
}


names(df) <- c("hospital", "state")




set.seed(6463)
Nd<-4000
U<-matrix(rnorm(Nd^2,mean=1,sd=2),nrow=Nd)
V<-matrix(rnorm(Nd^2,mean=3,sd=1),nrow=Nd)
Di<-runif(Nd,min=.2,max=6.3)
X<- U%*%diag(sort(Di))%*%t(V)


R Quiz 3

Q1 -
    mean(iris[iris$Species == "virginica", 1])
6.588
Q4 -
    
abs(mean(mtcars[mtcars$cyl == 4, 4]) - mean(mtcars[mtcars$cyl == 8, 4]))

n <- 1
nPlusOne <- 1
itr <- 1
fibSeq <- c(n, nPlusOne)

fib <- function (x) {
    for(i in 2:x) {
        tmp <- (n + nPlusOne)
        n <- nPlusOne
        nPlusOne <- tmp
        fibSeq <<- c(fibSeq, nPlusOne)
        ##print(fibSeq)
    }
}

itr <- 0

fib <- function (x, n = 1, nPlusOne = 1) {
    if (x < 1) {
        ##return (n + nPlusOne)
        return (NULL)
    } else {
        tmp <- (n + nPlusOne)
        n <- nPlusOne
        nPlusOne <- tmp        
        return (c(n, fib(x - 1, n, nPlusOne)))
    }
}




    f<-function(x) {
        if(x>2) {
            d<-1+f(x/2)
        } else {
            d<-1
        }
        d
    }


<<<<<<< HEAD
=======

>>>>>>> 3fa0048329fd769eb3b9af2d81a5903255427221
