## Creates an object that stores a
## matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}

## Returns the inverse of cacheMatrix x
## created by makeCacheMatrix
##
## If cached inverse exists, return it,
## otherwise, compute inverse and cache it

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("Using cached inverse")
    } else {
        inv <- solve(x$get(), ...)
        x$setInverse(inv)
    }
    inv
}
