########################################################
## Coursera - John Hopkins University - R Programming ##
########################################################
##
## cachematrix.R defines a set of functions and helper
## functions to efficiently calculate the inverse of a 
## matrix. 
##
## Functions:
## - cacheSolve(x, ...)
##
## Helper Functions:
## - makeCacheMatrix(x)
##
########################################################

## makeCacheMatrix takes an object of type matrix and
## binds to a private variable "m" that keeps the result 
## of the operation performed over matrix "x" and four 
## functions: 
## - set: set matrix "X" to "Y" and resets "m" to NULL
## - get: returns matrix "X"
## - setOpResult: assigns to internal variable "m" the 
##                value of argument "result"
## - getOpResult: returns the value of "m"
##
## list command binds the functions to input matrix 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function() {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setOpResult <- function(result) m <<- result
  getOpResult <- function() m
  list(set = set, get = get,
       setOpResult = setOpResult,
       getOpResult = getOpResult)
}


## cacheSolve is an efficient implementation for
## matrix inversion that makes use of helper function
## makeCacheMatrix to cache solve operation over input
## matrix, or retrieve the previously cached result in
## case the same matrix is given as input.
## To use this function matrices must be converted to
## a "cacheable" matrix form through function 
## makeCacheMatrix.
##
## Example: x_cache <- makeCacheMatrix(x)
##            x_inv <- cacheSolve(x_cache)
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getOpResult()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setOpResult(m)
  m
}
