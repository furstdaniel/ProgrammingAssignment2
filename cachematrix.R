# These functions are designed to create the invert of a matrix, and cache it,
# so when using next time, save processing time on skipping the calculation.

# This function is to create methods to set and get a matrix and the inverse of it 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# Calculate the inverse of a matrix and cache it, so when it is used for the second time, it
# can retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached inverse matrix")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
