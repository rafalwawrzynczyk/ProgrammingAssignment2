##  The pair of below functions enables the user to effectively
##  perform an inversion of matrix. Thanks to using a special
##  format of storing matrix' data, costly inversion operation
##  is done only if it is necessary (i.e. for the first time).
##  After the inverse has been calculated, it is to be accessed 
##  directly from cache.


##  Function 'makeCacheMatrix' creates a special 'matrix' object
##  that can cache its inverse.
##
##  This object is the list of four functions enabling
##  to access/set either the matrix (get/set) or its inverse
##  (getInverse/setInverse).

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


##  Function 'cacheSolve' returns a matrix that is the inverse of 'x'
##  where 'x' is a special 'matrix' object that can cache its inverse.
##
##  Function checks if inverse of 'x' is cached in 'x' - if so,
##  cached value is used instead of starting calculation.
##  Otherwise, we use R 'solve' function to calculate the inversion,
##  and store the result in 'x' before we return it as function's 
##  output.

cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setInverse(inv)
  inv
  
}
