## makeCacheMatrix function initialize the Cache and provide some interfaces
makeCacheMatrix <- function(x = matrix()) {
        ##initialize inversed matrix
        i <- NULL
        ##set matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ##get matrix
        get <- function() x
        ##set inversed matrix
        setInverse <- function(inverse) i <<- inverse
        ##get inversed matrix
        getInverse <- function() i
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve function return Inversed matrix if it has already be cached,
## else compute, cache and return
cacheSolve <- function(x, ...) {
        ## getInverse from cache list
        i <- x$getInverse()
        ## when inverse was cached, retrieve it
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## when data was NOT cached, compute, cache and return it
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
