## These functions reduce the amount of computation time. A matrix can be cached with
## 'makeCacheMatrix' and then retrieved or computed with 'cacheSolve'


## 'makeCacheMatrix' creates a matrix that can cache its inverse

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


## 'cacheSolve' returns a cache of the inverse matrix.  If not cached, will return the inverse.

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
