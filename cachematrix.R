## These functions store a matrix and cache its inverse.
## Changing the matrix clears the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  # Store a new matrix and clear the old inverse.
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # Retrieve the stored matrix.
  get <- function() x
  
  # Store the calculated inverse.
  setinverse <- function(value) inverse <<- value
  
  # Retrieve the cached inverse.
  getinverse <- function() inverse
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Return the cached inverse, or calculate and cache it.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}