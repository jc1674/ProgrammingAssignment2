## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # This will store the cached inverse

    # Set a new matrix and clear any cached inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    # Get the matrix
    get <- function() x

    # Set the inverse
    setinverse <- function(inverse) inv <<- inverse

    # Get the cached inverse
    getinverse <- function() inv

    # Return a list of the four functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix. If the inverse is already cached, it returns it
## directly; otherwise, it computes and caches it.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()

    # If the inverse is already cached, return it
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }

    # Otherwise, compute the inverse
    data <- x$get()
    inv <- solve(data, ...)  # solve() computes the inverse
    x$setinverse(inv)        # Cache the result
    inv                      # Return the inverse
}
