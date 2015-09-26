## This file contains functions for creating and solving a special matrix
## object that is able to cache said matrix's inverse

## makeCacheMatrix creates a special matrix object capable of remembering its
## inverse. This function takes a matrix as an argument and returns a list of
## functions for accessing the special matrix object

makeCacheMatrix <- function(mat = matrix()) {
    inverse <- NULL
    set <- function(newmat) {
        mat <- newmat
        inverse <- NULL
    }
    get <- function() mat
    setinverse <- function(newinverse) inverse <<- newinverse
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes in a matrix created via makeCahceMatrix, solves and assigns
## the inverse of that matrix. This functions takes in a list of functions for
## accessing a special cached matrix and returns the calculated inverse

cacheSolve <- function(mat, ...) {
    inverse <- mat$getinverse()
    if(!is.null(inverse)) {
        message("Getting cached inverse")
        return(inverse)
    }
    data <- mat$get()
    inverse <- solve(mat$get())
    mat$setinverse(inverse)
    inverse
}
