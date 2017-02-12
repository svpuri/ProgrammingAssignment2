## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  cachedInverse <- NULL
  
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) cachedInverse <<- inverse
  getinverse <- function() cachedInverse
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrixInverse <- x$getinverse()
  
  if(!is.null(matrixInverse)) {
    message("Getting cached matrix data....")
    return (matrixInverse)
  }
  
  matrixData <- x$get()
  matrixInverse <- solve(matrixData)
  x$setinverse(matrixInverse)
  matrixInverse
}