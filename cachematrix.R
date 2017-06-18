# Functions for managing the computation and caching of the inverse
# of a given matrix.

# Function that creates a special matrix object that caches both a matrix
# and its inverse.
# Returns a list containing the getters and setters for the cached values.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL  # invalidate the inverse since the matrix has changed
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


# Function that returns the inverse of a special matrix object.
# If the inverse does not exist, it it computed and cached n the object.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
