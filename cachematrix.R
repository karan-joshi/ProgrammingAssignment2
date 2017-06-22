## This solution is for Programming Assignment 2 for R Programming course
## The methods below are to compute and cache inverse of an invertible matrix. The cached inverse is
## used when available instead of computing a new one.

## makeCacheMatrix function creates a special vector which is essentially a list of functions to set 
## and get the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve function checks for the cached inverse 
## If cached inverse not available, then calculate and cache it

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}