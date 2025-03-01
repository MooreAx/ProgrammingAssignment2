## Calculates the inverse of a square (invertible) matrix and cache the result.


## CONTAINER FUNCTION for the matrix and its inverse with functions to get and
## set the  matrix, and store and retrieve its inverse. If the matrix changes,
## the cached inverse is cleared to stay up to date.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    # only do something if x,y are not identical
    if(!identical(x, y)){
      message("matrix change. erasing cache")
      x <<- y
      inv <<- NULL
    } else {
      message("no matrix change")
    }
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## OPERATOR FUNCTION that retrieves the cached inverse from the container, 
## if available. If no cache exists, it calculates the inverse and caches it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  message("no cache. calculting inverse")
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinverse(inv)
  inv
}

#testing function: generate random square matrix where nrows = ncols = size (default = 5x5)
rand_matrix <- function(size = 5){
  matrix(sample(0:9, size^2, replace = TRUE), size, size)
}
