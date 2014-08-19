## These two functions 'makeCacheMatrix' and 'cacheSolve' can be used to calculate
## a matrix inverse without having to re-calculate it's value repeatedly.
## 'makeCacheMatrix' creates a "matrix" object that can cache (store) its inverse 
## 'cacheSolve' accesses the "matrix object" created by makeCacheMatrix and returns
## the matrix inverse. If the inverse was previously calculated, it simply returns
## the cached (stored) value thus saving many calculation cycles
#######################################################################################

## makeCacheMatrix: This function creates a "matrix" object that can cache its inverse.
## It stores the matrix inverse so that it can be accessed later without a need to
## re-compute the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  xinv <- NULL
  # set() clears the xinv variable and sets the matrix x
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  # get() returns the value of matrix x
  get <- function() x
  
  # setinv() sets the value of the matrix inverse 'xinv'
  setinv <- function(inv_matrix) xinv <<- inv_matrix
  
  # getinv() returns the value of the matrix inverse 'xinv'
  getinv <- function() xinv
  
  # makeCacheMatrix returns a list with the functions defined above
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## 'cacheSolve' accesses the "matrix object" created by makeCacheMatrix and returns
## the matrix inverse. If the inverse was previously calculated, it simply returns
## the cached (stored) value thus saving many calculation cycles. 
## 'cacheSolve' uses functions inside the object created by 'makeCacheMatrix' to retrieve
## and/or set the matrix and its inverse

cacheSolve <- function(x, ...) {
  # Note that 'x' here is an object as created by 'makeCacheMatrix', so it has it's
  # own functions and environment variables
  inverse <- x$getinv()
  if(!is.null(inverse)) { # This code executed if the inverse was calculated before
    message("getting cached data")
    return(inverse)
  }
  # Following code executed if the inverse was never calculated before
  xmatrix <- x$get()
  inverse <- solve(xmatrix, ...)
  x$setinv(inverse)
  inverse # Returns the inverse matrix
}