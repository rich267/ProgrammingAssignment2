
## Creates a special "matrix" object that can cache its inverse.
## You can call
## * set(matrix) - stores the matrix
## * get() - returns the matrix
## * setinv(inv) - caches the inverse of the matrix
## * getinv() - returns the cached inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL ## initially nothing cached
  set <- function(y) {
    x <<- y ## store matrix
    minv <<- NULL ## clear cache since we've stored a new matrix
  }
  get <- function() x ## the matrix
  setinv <- function(inv) minv <<- inv ## cache the inverse
  getinv <- function() minv ## return the cached inverse
  
  ## return setter and getter for matrix and the inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Computes the inverse of the special "matrix" returned by 
## makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x', such that
  ## x %*% cacheSolve(x) = the identity matrix
  
  inv <- x$getinv() ## get cached value
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get() ## get matrix
  inv <- solve(data, ...) ## compute matrix inverse
  x$setinv(inv) ## cache the inverse
  inv ## return the inverse
}
