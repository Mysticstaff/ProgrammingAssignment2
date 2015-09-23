## This function was created for the Programming Assignment 2 of the
## Coursera R Programming class, 2015/09/23

## This function is almost exactly the same as the makeVec function.
## There are 4 functions: setMatrix, getMatrix, setInverse and getInverse.
## setMatrix 'caches' a matrix, getMatrix retrieves the cached matrix,
## setInverse (which is called by cacheSolve below) caches the matrix inverse,
## and getInverse retrieves the cached inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setMatrix <- function (y = matrix()){
      x <<- y 
      inv <<- null
  }
  getMatrix <- function () x
  setInverse <- function(inv) i <<- inv
  getInverse <- function() i
  
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This function is also almost the same as cachemean,
## except that the function calculates the inverse of the matrix
## instead of the mean. It is assumed that the matrix is square.
## Otherwise 'solve' may be unable to calculate the inverse.
## No particular code is added to check that the matrix is square.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cached matrix")
    return(i)
  }
  data <- x$getMatrix()
  i <- solve(data)
  x$setInverse(i)
  i
}
