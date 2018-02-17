## Put comments here that give an overall description of what your
## functions do
## Author: Pablo Sainz, Feb 2018

## Factory method that creates an object with the following methods:
##    -get(): Returns the actual matrix value 
##    -set(): Set the new value for the matrix
##    -getInverse(): Get the current value for the inverse of the provided matrix
##    -setInverse(): Set the new value for the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseValue <- NULL
  #Create the mutator
  set <- function(newMatrix){
    x <<-newMatrix
    inverseValue <<-NULL
  }
  
  #Create the accessor
  get <-function() x
  
  #Create the mutator for the inverse
  setInverse <- function(newInverse){
    inverseValue <<- newInverse
  }
  
  #Create the accessor for the inverse
  getInverse <- function() inverseValue
  
  #Finally create the complete matrix object
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Utility method that resolves the inverse of a given matrix with a 
## built-in cache. If the inverse has been already set in the object, then
## the calculation is skipped and the cached inverse matrix is returned instead.

cacheSolve <- function(x, ...) {
        ## Obtain the inverse from the object's cache
  currentInverse <- x$getInverse()
  #Check if it has been calculated
  if(!is.null(currentInverse)){
    message("Matrix inverse detected on cache")
    return(currentInverse)
  }
  else{
    #If no cached inverse matrix is detected then we calculate it
    matrixData <- x$get()
    if(!is.null(matrixData)){
      currentInverse <- solve(matrixData,...)
      message("Matrix inverse calculated")
      x$setInverse(currentInverse)
      return(x$getInverse())
    }
    else{
      warning("The matrix is not initialized properly, verify and try again")
    }
    
  }
    
}
