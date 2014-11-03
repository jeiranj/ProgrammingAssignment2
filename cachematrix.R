## These two functions cache a matrix and its inverse.
## If the matrix is large, computational cost of inversion 
## is heavy and caching the inverse allows for efficiency.

# This function creates the cache environment.
# Sub-functions, set, get, getinv and setinv have access to
# the cache environment, which contains the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                 # Set the inverse variable
    set <- function(y) {        # Cache matrix and its inverse
        x <<- y
        inv <<- NULL
    }
    get <- function() x 
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# This function computes and caches inverse of a cached matrix.
# If inverse has already been cached, it gets the cached inverse instead.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()            # get the cached inverse
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # if inv is empty and the inverse is not cahced:
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)                # cache the computed inverse
    inv
}
