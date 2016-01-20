## Put comments here that give an overall description of what your
## functions do

## This function creates a class that allows caching for a matrix and its
## inverse. It stores the matrix and its inverse; when the matrix is changed,
## the inverse is cleared and needs to be set by a call to cacheSolve, below.

makeCacheMatrix <- function(x = matrix()) {

  matrix_inverse <- NULL
  set <- function(y) {
    x <<- y
    matrix_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(new_inverse) matrix_inverse <<- new_inverse
  getinverse <- function() matrix_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function calculates the inverse of the special matrix created with the
## function above, if it is not cached. If it is cached, the cached version is returned instead, 
## speeding computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  the_inverse <- x$getinverse()
  if(!is.null(the_inverse)) {
    message("getting cached data")
    return(the_inverse)
  }
  data <- x$get()
  the_inverse <- solve(data, ...)
  x$setinverse(the_inverse)
  the_inverse
}
