## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## It has functions to set the matrix value, get the matrix value, set the inverse of the matrix and get the inverse of the input matrix


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


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.
## The cacheSolve function assumes that the input matrix can be inverted

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