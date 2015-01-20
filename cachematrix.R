##----------------------------------------------------------------------------------
## 
## Script exposing functions to fast inverse matrixes
## by implementing a cache-functionality
##
## Author:  Nicky Overgaard Pedersen
## Version: 1.0.0
## License: MIT
##
## Functions exposed
##  makeCacheMatrix(m = matrix()) 
##  cacheSolve(m = matrix())
##
##----------------------------------------------------------------------------------


makeCacheMatrix <- function(m = matrix()) {
##----------------------------------------------------------------------------------  
##  Args:
##    m: an atomic matrix
##  
##  Returns:
##    A special "matrix" object can cache its inverse (solved matrix)
##
##  Exposes:
##    set() sets the value of the matrix to be Inverted
##    get() gets the value of the matrix to be Inverted
##    setInverted() sets the value of the solved (Inverted) matrix
##    getInverted() gets the value of the solved (Inverted) matrix
##----------------------------------------------------------------------------------  
  
  ## make sure the inverted matrix is null'ed before we start anything else
  mInv <- NULL
  
  fSet <- function (y) {
    ##simply assign the value passed to the matrix (m)
    m <<- y
    
    ## when matrix is set, the Inverted (cached) matrix is per default invalidated
    ## - so null it now in parent environments.  
    mInv <<- NULL 
  }
  
  fGet <- function () {
    # simply return the matrix
    m
  }
    
  fSetInverted <- function (y) {
    # first set/update the cached result (inverted matrix)
    # then make sure m is updated to ensure data integrity
    mInv <<- solve(y)
    m <<- solve(mInv)
  }
  
  fGetInverted <- function () {
    ## if a cached result (inverse of m) can't be found, 
    ## then call fSetInverted(m) to solve/invers m and cache the result
    ## before returning the result
    if(is.null(mInv)) {
      fSetInverted(m)
    }
    mInv
  }
    
  ##return a list of exposed functions
  list( set = fSet, 
        get = fGet,
        setInverted = fSetInverted,
        getInverted = fGetInverted)
}


##---------------------------------------------------------------------------
##
## This function inverts (solves) the the special "matrix" created with the 
## function makeCacheMatrix
##
##---------------------------------------------------------------------------
cacheSolve <- function(m = maxtrix()) {
##----------------------------------------------------------------------------------  
##  Args:
##    m: an atomic matrix or a special "matrix" returned by makeCacheMatrix(matrix)
##  
##  Returns:
##    Returns the solved (inverted) matrix of m
##    if availabe, a cached version of the inverted m, will be returned 
##    to save processing time by avoiding calls to solve(matrix)
##
##----------------------------------------------------------------------------------  


  ## make sure m is a recursive matrix, to catch use cases like cacheSolve(matrix(c1:4,2,2)) 
  matrix <- m
  if (is.atomic(m)){
    m <- makeCacheMatrix(matrix)
  }

  ## return the inverse matrix of m
  ## m$gerInverted() will always return a cached inverse matrix of m
  return(m$getInverted())
}


##----------------------------------------------------------------------------------  
##
## TEST CASES
##
##----------------------------------------------------------------------------------  
##matrix <- matrix(1:4,2,2)
##m1 <- makeCacheMatrix(matrix)
##m1Inv <- cacheSolve(m1)
##m2Inv <- cacheSolve(matrix)
##m3Inv <- solve(matrix)
##m4Inv <- m1$getInverted()
