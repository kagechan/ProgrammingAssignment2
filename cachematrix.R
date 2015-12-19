## cacheMatrix
## Description: These functions are useful when you
## calculate the inverse of a matrix. The calculation of
## matrix is time-consuming, so the makeCacheMatrix
## stores the inverse of the specified matrix into the
## memory as a cache, in order to make your calculation
## fast.
## How to use: you can call
## cMat <- makeCacheMatrix(mat)
## cachesolve(cMat)
## ...

## makeCacheMatrix: 
## The environment for storing the result of inverse of
## the specified matrix into a cache.
## setinverse: set the calculated inverse matrix.
## getinverse: get the cached inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: returns the inverse of matrix
## Note: This function can be called *after* you called
## the makeCacheMatrix function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  ## if the environment already contains cached data,
  ## return them at once.
  if (!is.null(i)) {
    message("Getting cached data...")
    return(i)
  }
  ## if doesn't, calculate the inverse of 'x'
  ## and set the result into the environment.
  i <- solve(x$get())
  x$setinverse(i)
  i
}
