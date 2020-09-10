## These functions work together to handle a special "matrix" ## (similar to a 
## new class that has a inverted matrix attribute). makeCacheMatrix creates the 
## matrix with its functions (get,set,getinv and setinv) and cacheSolve is used
## for calculate and save in the cache the inverse of the matrix.


makeCacheMatrix <- function(mat = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse.
  
  inv <- NULL
  set <- function(new_mat) {
    mat <<- new_mat
    inv <<- NULL
  }
  get <- function() mat
  setinv <- function(new_inv) inv <<- new_inv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


cacheSolve <- function(x, ...) {
  ## This function computes the inverse of the special "matrix" returned 
  ## by makeCacheMatrix. If the inverse has already been calculated (and the 
  ## matrix has not changed), then the cachesolve should retrieve the inverse 
  ## from the cache.
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}
