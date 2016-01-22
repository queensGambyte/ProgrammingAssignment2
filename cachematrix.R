## The functions makeCacheMatrix and cacheSolve together calculate and cache the inverse 
## of a matrix.

## makeCacheMatrix returns a matrix like object from a matrix x, which servers 
## as input to cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes the object created by makeCacheMatrix function, returns its inverse
## and caches it. When called on the same object again, it skips the calculation and 
## returns the cached data.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
