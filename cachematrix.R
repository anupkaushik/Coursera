## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize local variable inv to NULL so we can tell if cacheSolve has run at least once 
  inv <- NULL
  set <- function(y) {   ## create set function to store the matrix passed in the call 
    x <<- y              ## storing the matrix in cache  
    inv <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  ## invert the matrix and store in cache
  setInverse <- function() inv <<- solve(x) 
  ## get the inverse matrix from cache
  getInverse <- function() inv
  
  ## return the created functions to working environment 
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## attempt to get the inverse of the matrix from cache
  
  inv <- x$getInverse()
  ## return inverted matrix from cache if it exists 
  ## else create the matrix in working env
  
  if(!is.null(inv)) {
    message("getting inverse from Cache")
    return(inv)
  }
  ## create the matrix in working env
  mat <- x$get()
  ## inverse the matrix 
  inv <- solve(mat, ...)
  ## set inverted matrix in cache
  x$setInverse(inv)
  inv
}

