## This set of functions is used to store a special "matrix" object and then
## cache its inverse. This is advantageous because computing the inverse of a
## matrix is time consuming. Being able to retrieve the inverse from memory
## quickly can end up saving a lot of time.

## makeCacheMatrix takes a square invertible matrix, X, and stores it as a
## special object that can be used to cache the inverse of X.

#1
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix object create by makeCacheMatrix.
## If the inverse has already been calculated, it will be retrieved from the
## cache instead of being computed by the solve function.
#2
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
