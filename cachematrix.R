## Put comments here that give an overall description of what your
## functions do

## Similar to the makeVector function, this function is responsible for creating a matrix that can
## maintain a cached inverse of itself.  It holds two variables: x, which is the underlying matrix and
## i, which is the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This is the primary function to evaluate if the inverse of a matrix created by makeCacheMatrix
## contains a cached inverse already.  If so, it merely returns the already evaluated inverse.
## Otherwise, it evaluates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
