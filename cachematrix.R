## Methods to cache the inverse of a matrix and to obtain the cached version of
## said matrix

## on first call to obtain the matrix inverse, cache the inverse so all 
## subsequent calls just return a copy
makeCacheMatrix <- function(x = matrix()) {
  cached_inverse <- NULL
  set <- function(y) {
    x <<- y
    cached_inverse <<- NULL
  }
  
  get <- function() { x }
  setinverse <- function(inverse) { cached_inverse <<- inverse }
  getinverse <- function() { cached_inverse }
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## method to return the inverse of a matrix, while ensureing that we cache
## the result
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  
  if(!is.null(inverse)) {
    message("getting cached data.")
    return (inverse)
  }
  
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  
  inverse
}