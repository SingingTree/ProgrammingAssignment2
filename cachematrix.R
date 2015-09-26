## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
