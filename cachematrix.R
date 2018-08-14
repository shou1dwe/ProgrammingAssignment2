## Providing quick functions to create a special matrix wrapper that can cache inverse
## value of the underlying matrix and the corresponding solve function that retrieve 
## cached inverse of the matrix directly if it is available.

## create a special wrapper of matrix that has API for caching inverse value of it
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## solve the matrix and return the inverse and cache the result. 
## if a cahced inverse can be found, return the cached value directly
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
