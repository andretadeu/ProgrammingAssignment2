## These functions returns the inverse of a matrix, using cache to
## avoid recomputation.

## Usage:

## mat <- /* create your matrix */

## Creating a cached matrix:
## cacheMatrix <- makeCacheMatrix(mat)

## Running cacheMatrix$getInverse() now returns null, since the inverse of
## the matrix is not calculated yet

## The following command calculate the inverse of the matrix using 'solve'

## inv1 <- cacheSolve(cacheMatrix)

## Now it is possible to check the inverse matrix cached

## inv2 <- cacheMatrix$getInverse()

/*---------------------------------------------------------------------------*/

## Creates a cache for matrix.

makeCacheMatrix <- function(x = matrix()) {
    ## Returns the cache of a matrix
    inv <- NULL
    # 'Setter' for the cached matrix
    setMatrix <- function(mat) {
        x <<- mat
        inv <<- NULL
    }
    # 'Getter' for the cached matrix
    getMatrix <- function() x
    # 'Setter' for the cached inverse matrix
    setInverse <- function(solve) inv <<- solve
    # 'Getter' for the cached inverse matrix
    getInverse <- function() inv
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse, getInverse = getInverse)
}


## This function returns the inverse of the matrix.
## If the inverse of the matrix is cached, returns it, else call 'solve' to the
## matrix and cache the result for a future use.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## param: x - a cacheMatrix returned from makeCacheMatrix
    inv <- x$getInverse()
    # If the result is cached, returns it
    if (!is.null(inv)) {
        return(inv)
    }
    # Calculate the inverse of the matrix
    mat <- x$getMatrix()
    inv <- solve(mat)
    x$setInverse(inv)
    inv
}
