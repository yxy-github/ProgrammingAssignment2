## Caching the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setInverse <- function(inverse) inv <<- inverse
     getInverse <- function() inv
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function returns a matrix that is the inverse of a special matrix 'x'.
## It computes the inverse of 'x' returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),  
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
     inv <- x$getInverse()
     if (!is.null(inv)) {
          message("Getting cached data ...")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setInverse(inv)
     inv     
}
