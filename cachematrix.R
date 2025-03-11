## This code defines two functions that cache the inverse of a matrix 
## to avoid redundant computations

## This function creates a special object that stores a matrix 
## and caches its inverse, we can do:
## - set(): Updates the matrix and resets the cached inverse.
## - get(): Retrieves the stored matrix.
## - setinverse(): Stores a precomputed inverse.
## - getinverse(): Retrieves the cached inverse.


makeCacheMatrix <- function(x = matrix()) {
     inverse<-NULL #inverse matrix that we want to cache
     set <- function(y) {
          x <<- y
          inverse <<- NULL # Reset cached inverse when setting a new matrix
     }
     
     get <- function() x
     setinverse <- function(inv) inverse <<- inv # 'inv' must be the precomputed 
     ## inverse, for example, inv = solve(makeCacheMatrix$get())
     getinverse <- function() inverse
     
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function computes the inverse of the matrix stored in a 
## 'makeCacheMatrix' object, if the inverse is already cached, it returns the
## cached value instead of recomputing.

cacheSolve <- function(x, ...) { 
     
     m <- x$getinverse()
     if(!is.null(m)) {
          return(m)
     }
     
     matrix_use <- x$get()
     m <- solve(matrix_use)
     x$setinverse(m)
     m 
}
