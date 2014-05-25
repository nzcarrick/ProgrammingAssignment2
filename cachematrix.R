## this file contains funtions to allow the caching 
## of inverase matrix functions

## Attributution to RDPANG as per fork.

## This function creates a cache of a matrix to improve speed.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(cacheSolve) m <<- cacheSolve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## this function calulates the inverse of a matrix using
## using the cache as a source of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data) %*% data
        x$setinv(m)
        m
}
