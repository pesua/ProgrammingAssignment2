## This functions are intended to avoid repetitive computation of inverse matrix.
## Use makeCacheMatrix(matrix) to create a wrapper that is responsible for caching.
## Use cacheSolve to get the inverse of matrix.

## Exaple:
## m <- matrix( c(4,3,3,2), nrow=2, ncol=2) 
## mc <- makeCacheMatrix(m)
## cacheSolve(mc)
## cacheSolve(mc)


## Creates matrix wrapper that can cache inverse value.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
