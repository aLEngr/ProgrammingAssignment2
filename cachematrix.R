## The following functions are being written to save the inverse
##  of a matrix

## This f(x) creates a special "matrix" object called X, that can save its inverse

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
            
      }
      get <- function() x
      setinverse <-function(inverse) i <<- solve(x)
      getinverse <- function() i 
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse )
}

## this (helper) function computes the inverse of the special "matrix" returned
## by the above function via a loop so we dont have to calculate each time. 


cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return (i)
            
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}

