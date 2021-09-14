## Below are two functions makeCacheMatrix() and cacheSolve()
## makeCacheMatrix() return a list of methods that can be used to store 
##                   in and retrieve from both an the parent environment.
## cacheSolve() is used to generate the inverse matrix and uses makeCacheMatrix
##               to store a caches version allow for fast subsequent retrieval
##
## To Test:
##
## source("cachematrix.R")
## m1 <- matrix(rnorm(9), ncol=3)
## mf <- makeCacheMatrix(m1)
## cacheSolve(mf) ## initial call - generate cached data
## cacheSolve(mf) ## subsequent call - retrieve from cached data


## makeCacheMatrix function
## x is the stored original matrix data
## m is the stored inverse matrix data (initially NULL until setmatrix() is called
## functions:
## get() will return the original matrix data
## setmatrix() will set the inverse matrix data
## getmatrix() will return (returns NULL if not set)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}

## cacheSolve function
## Attempt to retrieve inverse matrix data from cache
## if found, return the inverse matrix directly
## otherwise (if not found), 
##   1) retrieve original matrix data, 2) inverse it with solve(),
##   3) store inverse matrix using $setmatrix(), 4) return inverse matrix
## Note: Original matrix (x) must be a square matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    message("generating new cached data")
    data <- x$get()
    m <- solve(data)
    x$setmatrix(m)      
    m
}
