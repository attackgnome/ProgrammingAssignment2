## Create a class of matrix that can cache its inverse and the 
## corresponding function solve for the inverse of that matrix
## using ginv() from the MASS package

require(MASS)

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  require(MASS)  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  get <- function() x
  setinverse <- function(ginv) m <<- ginv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- ginv(data, ...)
  x$setinverse(m)
  m
  
  
  
}
