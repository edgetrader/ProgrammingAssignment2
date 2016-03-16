## R Programming Assignment 2 (Week 3) by edgetrader
## 16 March 2016


## makeCacheMatrix is a function that creates a Cache Matrix 
## that can store its inverse and itself in Cache

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL

    # Function to store Input Matrix in x in the Cache Environment
    # Initialise m for storing the Inverse of Input Matrix in the Cache Environment
    setM <- function(y) {
        x <<- y
        m <<- NULL
    }

    # Function to return the Input Matrix from the Cache Environment
    getM <- function() x

    # Function to set the Inverse Matrix in the Cache Environment
    setInverse <- function(Inverse) m <<- Inverse

    # Function to return the Inverse Matrix in the Cache Environment
    getInverse <- function() m

    # Return list of 4 functions
    list(setM = setM, getM = getM,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve is a function that calculates the Inverse of the Input Matrix
## one time and then stores it in Cache.  And then subsequently, return
## the Inverse value from Cache without any computation.


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    m <- x$getInverse()

    # Check Cache and Return Cache Value if not empty
    if(!is.null(m)) {
        message("Reading from Cache.  No Calculation.  More Efficient.")
        return(m)
    }

    # Calculate the Inverse (First Time)
    data <- x$getM()
    m <- solve(data, ...)

    # Store the Inverse in Cache
    x$setInverse(m)

    # Return the Inverse matrix
    m    
    
}
