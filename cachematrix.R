## Put comments here that give an overall description of what your
## functions do

## function that stores a matrix and its inverse. It calcs the inverse
##  if it doesnt already exist and caches the value to be returned later.
##
##    Function takes one parameter and returns a list with four members:
##    param x, the matrix assigned to it, default an empty matrix
##    returns a list containing four functions:
##       set, a function that allows replacing the assigned matrix
##       get, function to return the assigned matrix
##       setinv, function to set the inverse matrix 
##       getinv, function to get the inverse matrix
## 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(val) inv <<- val
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## function that stores a matrix and its inverse. It calcs the inverse
##  if it doesnt already exist and caches the value to be returned later.
##
##    Function takes one parameter and returns one value:
##    param x, list returned by the makeMatrix function, must be set
##    returns the inverse of the matrix that is either already 
##      cached, or it is calculated and then cached.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

