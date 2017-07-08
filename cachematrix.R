## First of all, you excuse my written English if it is not correct enough.
## I'll try to explain my code as best I can

## The makeCacheMatrix  Function  has an input a Matrix (a square invertible matrix)
## the result of makeCacheMatrix is the input of th e other function cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  
  #The result variable is the inverse matrix of result. I set to null
  result <- NULL
  
  #Set a matrix to x variable
  set <- function(y) {
    x <<- y
    result <<- NULL
  }
  
  #get de data, in this case the matrix
  get <- function() x
  
  #set de inverse martix
  setInverse <- function(solve) result <<- solve

  #get de inverse matrix
  getInverse <- function() result
  
  # the final reurn is a list with the function set() get() setInverse() getInverse()
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve function return de Inverse Matrix, 
##the second time to execute it will return a cache of the result 

cacheSolve <- function(x, ...) {
  
  result <- x$getInverse()
  
  #In the case that result is not null, I return the result of Inverse Matrix 
  if(!is.null(result)) {
    message("getting cached data")
    return(result)
  }
  
  #In the case that the result is null
  # It mean that is the first time, I do all the process to get the inverse matrix 
  
  
  #get the data of the original Matrix.
  matrixData <- x$get()
  
  #the result is the Inverse of the matrix
  result <- solve(matrixData, ...)
  
  # I Set the Inverse of the matrix in x
  x$setInverse(result)
  
  # Finally I return the result variable that is the inverse matrix
  result
}