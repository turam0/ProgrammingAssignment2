## Two related functions for caching a matrix and its inverse
## makeCacheMatrix:  takes a matrix as an arguement and will provide utility to store the inverse
## cacheSolve:  Uses the makeCacheMatrix to return the inverse of the matrix.  
##    The inverse is cached by cacheSolve on first use and the cached value is returned in subsequent calls

## Creates an object which can store a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Uses the cachematrix object from the function makeCacheMatrix and manages its cache
## On first use - the inverse of the matrix is calculated and stored in the cachematrix
## On subsequent uses, the cached inverse will be returned

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m  
}
