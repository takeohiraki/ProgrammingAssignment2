##makeCacheMatrix takes a matrix supplied by the user that is a square invertible matrix, performs a 
##function (solve) that returns its inverse (the setinverse component of the code), and then caches the 
##result using getinverse and the list()

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##cacheSolve checks to see whether there is a cached result for the inverse of the user-provide matrix.
##If m is null as a result of there being no cached result (checked by !is.null), then cacheSolve 
##calculates the inverse. If there is a cached result, a message is printed, as well as the result
##that was cached.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
