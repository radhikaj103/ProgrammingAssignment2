## Creating an example of caching variables to speed up computing the inverse of a matrix

## makeCacheMatrix function creates a special matrix object that contains a list of functions 
## to cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL 
  setMatrix <- function(y){ # create function to set the value of x 
    x <<- y 
    inverse <<- NULL 
  }
  getMatrix <- function() x # create function to return the value of x
  
  setInverse <- function(inv) inverse <<- inv # function to set the value of inverse 
  
  getInverse <- function() inverse # function to get the value of inverse
  
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve function computes the inverse of the special matrix object created in the previous function
## cacheSolve first checks if the inverse has already been computed and stored in the cache variable, 
## if not, it computes and sets the inverse to the cache variable

cacheSolve <- function(x, ...) {
  matInv <- x$getInverse()
  if (!all(is.na(matInv))) {
    print("using cache value")
    
  } 
  else {
    mat <- x$getMatrix()
    matInv <- solve(mat)
    x$setInverse(matInv)
  }
  
  return(matInv)
  
  ## Return a matrix that is the inverse of 'x'
}
