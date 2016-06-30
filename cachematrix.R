## Put comments here that give an overall description of what your
## functions do

## cache the matrix orinial or inverse when solved

makeCacheMatrix <- function(x = matrix()) {
    matrixinv <- NULL ##value assigned at current (parent) level
    
    set <- function(y) { ## closure function
        x <<- y
        matrixinv <<- NULL ##assign NULL to the final result variable by modifying the parent value assignent
    }
    get <- function() x

    setInverse <- function(inverse) matrixinv <<- inverse
    
    getInverse <- function() matrixinv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Get matrix from parent and solve/inverse the original matrix from parent

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ## Return a matrix that is the inverse of 'x'
    matrixinv <- x$getInverse()
    if (!is.null(matrixinv)) {
        message("getting cached data")
        return(matrixinv)
    }
    originalmatrix <- x$get()
    matrixinv <- solve(originalmatrix, ...)
    x$setInverse(matrixinv)
    matrixinv
}
