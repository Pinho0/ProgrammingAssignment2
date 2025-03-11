## On this code we will find two function that can save the inverse of a matrix
## as cache in order to avoid the repetition of the same calculation

## On this function we can set our matrix to other, get the currently matrix 
## in use, set it's inverse and store that matrix as cache, and get that 
## inverse matrix

makeCacheMatrix <- function(x = matrix()) {
     inverse<-NULL #inverse matrix that we want to cache
     set <- function(y) {
          x <<- y
          inverse <<- NULL
     }
     
     get <- function() x
     setinverse <- function(inv) inverse <<- inv ## inv need to already be the
     ## inverse of the matrix, for example, inv = solve(makeCacheMatrix$get())
     getinverse <- function() inverse
     
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function check if the inverse of the matrix is on cache, if it is not,
## perform the matrix inversion and then save that in cache

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
