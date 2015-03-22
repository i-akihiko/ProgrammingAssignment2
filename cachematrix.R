## This function returns the inverse of a matrix
## The result is cached so that the next the result comes back faster

## the following function creates a matrix object which can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  ##$set(a) sets matrix a
  get <- function() x
  ##$get() returns matrix a
  setInverse <- function(solve) s <<- solve
  ##$setInverse(solve(a)) sets the inverse of matrix a to "s"
  getInverse <- function() s
  ##$getInverse(a) returns the inverse of matrix a ("s"). The inverse should be set before running this function 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The next function shows the inverse of a matrix from the cache of the above
## If there is no corresponding cache, it calculates the matrix's inverse and returns it. The result is cached.

cacheSolve <- function(x, ...) {
  s <- x$getInverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
    ## if the value of s exists, returns its inverse by using makeCacheMatrix
  }
  data <- x$get()
  s <- solve(data, ...)
  ## calculates the inverse because the result has not been stored as cache
  x$setInverse(s)
  ## sets the calculated inverse to makeCacheMatrix function as cache
  s
  ## returns the inverse
}
