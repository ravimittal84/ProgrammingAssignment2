## These functions can be used to inverse a matrix, which is a costly computation
## hence we have added caching, which will increase performance by returning
## cached results (if results were cached and matrix has not changed)

## This function will create a vector with 4 functions
## set will set the matrix
## get will return the matrix
## setInv will set the inverse of matrix
## getInv will get the inverse of matrix
## These internal functions will be used to get or set the matrix (or inverse)

makeCacheMatrix <- function(x = matrix()) {
  ## set the value of inv to null
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Returns inverse of matrix by computing it the first time and
## returning cached value subsequently.
## This function will require the above function to be passed as an argument
## along with the matrix (if matrix value was not set earlier)

cacheSolve <- function(x, ...) {
  Inv <- x$getInv()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  
  Inv <- solve(x$get())
  x$setInv(Inv)
  Inv
}
