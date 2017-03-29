## Scope of this R files include to create two functions 1) to Create a
## Cache matrix 2) calculate the inverse of the matrix and store in global 
## environment. These functions, will be used to retrieve the inverse a of a
## matrix when it is calculated before without recalculation. There by saving
## computing time


## Create a Cache Matrix, Inverse will be set to NULL when the data is firs set

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculate the Inverse of the CacheMatrix. When calculated, and data is not
## changed (CacheMatrix) then Inverse is retrieved from Cache.

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
