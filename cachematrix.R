## This file contains two functions, makeCacheMatrix(m = matrix(m, ...)) and 
## cacheSolve(m, ...) which are described below.

## This function creates a special "matrix" object that can cache its inverse.
## 'm' is the input matrix.

makeCacheMatrix <- function(m = matrix(m)) {
    inverse <- NULL
    
    ## Set a new matrix.
    ## 'y' is the new matrix.
    setMatrix <- function(y) {
        m <<- y
        inverse <<- NULL
    }
    
    ## Get the stored matrix.
    getMatrix <- function() m
    
    ## Set the inverse of the matrix.
    ## 'inv' is the inverse.
    setInverse <- function(inv) {
        inverse <<- inv
    }
    
    ## Get the inverse of the matrix.
    getInverse <- function() {
        inverse
    }
    
    ## Return a list containing the functions created by makeCacheMatrix(m).
    list(setMatrix = setMatrix, getMatrix = getMatrix, 
         setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the matrix returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then this function retrieves the inverse 
## from the cache.
## 'm' is the matrix to calculate the inverse of.

cacheSolve <- function(m, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Determine if inverse is already cached.  If so, return the inverse.
    inverse <- m$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- m$getMatrix() 
    inverse <- solve(data, ...) # Calculate the inverse of the matrix.
    m$setInverse(inverse) # Cache the inverse for next time.
    inverse
}