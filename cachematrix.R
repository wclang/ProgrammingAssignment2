## Programming Assignment 2 for the 
## Johns Hopkins Coursera R Programming course
##
## Code adapted from example given in the
## assignment instructions.
##
## William Christopher Lang
## April 6, 2016

## this function creates a matrix object that
## caches its inverse once its inverse has been computed
makeCacheMatrix <- function(X = matrix()) {
    inv <- NULL
    set <- function(Y) {
        X <<- Y
        inv <<- NULL
    }
    get <- function() X
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## this function computes the inverse of the matrix
## only if the inverse has not been cached.
cacheInverse <- function(X, ...) {
    inv <- X$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- X$get()
    inv <- solve(data, ...)
    X$setinverse(inv)
    inv
}
