## These functions cache the value of the inverse of a matrix so that when it's needed
## again it can be looked up in the cache rather than recalcuated.

## This function creates a special "matrix" and uses the <<- operator to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() 
    x
  setinverse <- function(solve) 
    m <<- inverse
  getinverse <- function() 
    m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## this function retrieves the calculated inverse of the matrix from the cache if 
##the matrix has not changed
## if the matrix has changed, it computes the inverse

cacheSolve <- function(x, ...) {
        
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
