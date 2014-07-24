## This collection of functions provides a special matrix object that can create
## and cache its inverse. This is useful when the computation of the inverse of
## a matrix may be required many times before the matrix is changed (e.g., in a
## loop).

## This function creates the special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  # Initialize the inverse of the matrix
  i <- NULL

  # Function to set the matrix data
  set <- function(y) {
    x <<- y
    i <<- NULL
  }

  # Function to get the matrix data
  get <- function() x
  
  # Function to set the inverse data
  setinverse <- function(inverse) i <<- inverse

  # Function to get the inverse data
  getinverse <- function() i

  # List of available functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve will retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {

  ## Return the cached inverse if it has already been computed.
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }

  ## Compute inverse using solve()
  data <- x$get()
  i <- solve(data, ...)

  ## Cache the inverse
  x$setinverse(i)

  ## Return a matrix that is the inverse of 'x'
  i
}
