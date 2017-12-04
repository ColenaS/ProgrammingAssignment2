## The following functions together can inverse a matrix and retrun the results. Using the 
## cached results if available. 
## The function assume the matrix provided can be inverted

## The makeCacheMatrix function creates functions to get and inverse the passed matrix

makeCacheMatrix <- function(x = matrix()) {
  mm <- NULL
  set <- function(y) {
    x <<- y
    mm <<- NULL
  }
  getIn <- function() x
  setinverse <- function(inver) mm <<- inver
  getinverse <- function() mm
  list(set = set, getIn = getIn,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This cacheSolve function computes the inverse of a matrix. If the inverse has already been calculated without
## any changes the cached value is returned

cacheSolve <- function(x, ...) {
  mm <- x$getinverse ()
  if(!is.null(mm)) {
    message("getting cached data")
    return(mm)
  }
  dataIn <- x$getIn()
  mm <- solve(dataIn)
  x$setinverse(mm)
  mm
}
