## This file contains two functions to help calculate the inverse of a SQUARE 
## matrix of any size.

## This method takes in an input matrix (the default input matrix is 1 x 1)
## and caches it.
## It handles the following:
## 1. set the value of a matrix
## 2. get the value of a matrix
## 3. set the value of the inverse of the above matrix
## 4. get the value of the inverse of the above matrix
makeCacheMatrix <- function(x = matrix()) {
  set <- function(y) {
    x <<- y
  }
  get <- function() { x }
  if (det(x) == 0) {
    print("Cannot compute inverse of matrix because it's determinant is ZERO.")
    print("Only the input matrix has been cached.")
    list(set = set, get = get)
    return
  }
  getinverse <- function() { x_inv <<- solve(x) }
  setinverse <- function() { x_inv }
  list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}


## This method takes in an input matrix x and returns it's inverse cached matrix
## If the inverse matrix is present in the cache, it is returned.
## If the inverse matrix is not cached, it computes the inverse and then caches
## it, before returning it
cacheSolve <- function(x, ...) {
  x_inv <- x$getinverse()
  if(!is.null(x_inv)) {
    print("Retrieving value of inverse of matrix x from cache...")
    return(x_inv)
  }
  ## the lines below will execute if cached inverse matrix is not found
  print("Computing inverse...")
  # fetching the input matrix
  x_mat <- x$get()
  # using 'solve' to compute to matrix inverse
  x_inv <- solve(x_mat)
  # caching the inverse matrix
  x$setinverse(x_inv)
  # returning the computed inverse matrix
  print("The inverse of the input matrix is below:")
  x_inv
}
