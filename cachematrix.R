

## this function create a special matrix which can cache its inverse. The special matrix is a list of 4 
## named component: get and set (accessors), setinverse (compute the inverse of the matrix via the 
## solve function and cache it) and getinverse (return the cache value of the matrix's inverse if cached or NULL
## otherwise)

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## this function computes the inverse of the matrix and return it cache value, if alreasy computed.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
