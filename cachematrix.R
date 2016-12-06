##Caching the inverse of the matrix
##this project contains 2 functions to achive the caching of inverse of the matrix
##since inversing of matrix is time consuming, it will be useful to cache it

## MakeCacheMatrix : This function creates a special 'Matrix' object that can cache the inverse

makeCacheMatrix <- function(x = matrix()) {
   
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setInverse <- function(inverse) inv <<- inverse
     getInverse <- function() inv
     list (set = set, get =get, setInverse= setInverse, getInverse = getInverse)
}


## This function computes the inverse of the matrix if its not already cached.
## if the matrix is not changed and the inverse is already in the cache, it will retrive it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
         message("getting cached data")
         return(inv)
    }
    
    mat <- x$get()
    inv <- solve(mat,...)
    x$setInverse(inv)
    inv
}
