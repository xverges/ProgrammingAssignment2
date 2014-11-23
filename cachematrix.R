## Wrap invertible matrixes with methods that cache their solved inverse
## Part of Programming Assignemnt #2

## Return a list of functions to manage the wrapped matrix

makeCacheMatrix <- function(x = matrix()) {
    invert <- NULL
    set <- function(y) {
        x <<- y
        invert <<- NULL
    }
    get <- function() x
    setinvert <- function(solved) invert <<- solved
    getinvert <- function() invert
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)
}


## Get the invert, using the cache if available or solving and caching it if not

cacheSolve <- function(x, ...) {
    invert <- x$getinvert()
    if(!is.null(invert)) {
        message("getting cached data")
        return(invert)
    }
    data <- x$get()
    invert <- solve(data, ...)
    x$setinvert(invert)
    invert
}
