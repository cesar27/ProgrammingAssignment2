## This code is a couple of functions that cache and compute the inverse 
## of a matrix.

## Function creates a special "matrix" object.

makeCacheMatrix <- function(m = matrix()) {
  inverso <- NULL
  set <- function(x) {
    m <<- x;
    inverso <<- NULL;
  }
  get <- function() return(m);
  setinv <- function(inv) inverso <<- inv;
  getinv <- function() return(inverso);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}



## This function calculates the inverse of the special "matrix" 
## returned by function makeCacheMatrix.

cacheSolve <- function(m, ...) {
  inverso <- m$getinv()
  if(!is.null(inverso)) {
    message("getting cached data")
    return(inverso)
  }
  data <- m$get()
  invserso <- solve(data, ...)
  m$setinv(inverso)
  return(inverso)
}

