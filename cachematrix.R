## Caching the inverse of a matrix

## "makeCacheMatrix" creates a "matrix": a list containing a function to set and get the matrix, as well as set and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## "cacheSolve" calculates the inverse of the "matrix" created with the above function.
## If the inverse has already been calculated, it gets the inverse from the cache and skips the calculation.
## Otherwise, it calculates the inverse and sets the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached matrix inverse")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
}
