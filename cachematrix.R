## These functions will find a matrix's solution in a cache or
## invert the matrix for the first time and cache the value

## This function makes a list of a matrix and it's solution if known

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setinv <- function(invert) m <<- invert
        getinv <- function() m
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}
## This function will check for a matrix's inversion solution or if not known
## calculate the value and store it for later use using the above function

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        
        ## Return a matrix that is the inverse of 'x'
        m
}
