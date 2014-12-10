##The first function, makeCacheMatrix, creates a special "vector" containing
##four functions to cache a matrix and its inverse, and the second function,
##cacheSolve, uses as input that produced vector to obtain the matrix's inverse

##It receives a matrix and initializes the inverse to NULL,
##defines four functions, one for setting the matrix in the cache,
##another for getting the matrix from the cache, the third one
##for storing the inverse in the cache and the last one for obtaining
##the cached inverse. 
##When the matrix is set in the cache, the inverse is set again to NULL.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## First checks if the inverse is already in the cache and returns it if so,
## otherwise, it computes it, stores it in cache and finally it returns it.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
