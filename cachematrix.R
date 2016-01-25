## Put comments here that give an overall description of what your
## functions do

## These functions are paired: makeCacheMatrix creates a list of 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # sets the inverse of the x matrix as NULL
  xInv <- NULL
  # defining the set function with free variable y
  set <- function(y) {
    # x and xInv stored in the parent namespace
    x <<- y
    xInv <<- NULL
  }
  # sets the get variable with x as the function
  get <- function() x
  # sets the setinverse as solve in the parent namespace
  setinverse <- function(solve) xInv <<- solve
  # sets getinverse as xInv
  getinverse <- function() xInv
  # creates a list with the named elements to feed cacheSolve
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# checks to see if there's a cached inverted matrix, then return either that cached matrix inversion
# or inverts the newly supplied matrix. This solution only works for one matrix at a time.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xInv <- x$getinverse()
  if(!is.null(xInv)) {
    message("getting cached data")
    return(xInv)
  }
  data <- x$get()
  xInv <- solve(data, ...)
  x$setinverse(xInv)
  xInv
}
