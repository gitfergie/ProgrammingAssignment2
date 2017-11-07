## Matrix inversion is a costly computation and it can be helpful to cache
## the inverse of a matrix rather than compute it repeatedly. The below
## functions create the inverse of a matrix and enable the use of that inverse
## when it has already been computed.

## makeCacheMatrix is a function that creates a special matrix object 
## that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## cacheSolve is a function that computes the inverse of the matrix returned
## by makeCacheMatrix. If the inverse has already been calculated the inverse will
## be retrieved from the cache saving computational time.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)){
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  return(inv)
}
