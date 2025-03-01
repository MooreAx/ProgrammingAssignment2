## Put comments here that give an overall description of what your
## functions do

## Creates a special vector, which is actually a list, with functions to:
# 1) set the values of a matrix (provided there is a change)
# 2) get the values of a matrix
# 3) set the inverse
# 4) get the inverse

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

## Calculates the inverse of the special "matrix" created with makeCacheMatrix

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
