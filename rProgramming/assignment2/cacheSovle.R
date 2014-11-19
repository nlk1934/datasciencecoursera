# define you matrix: x <- makeCacheMatrix((matrix(c(1,2,3,4), nrow=2, ncol=2)))
# then get your matrix by: x$get()
# cache your inversed matrix by: cacheSolve(x)

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
