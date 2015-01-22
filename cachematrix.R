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
##    getInverted() gets the value of the solved (Inverted) matrix
##----------------------------------------------------------------------------------  
  
  ## make sure the inverted matrix is null'ed / initialized before we start anything else
  mInv <- NULL
  
  fSet <- function (y) {
    ## don't spend CPU time on variable assignments and solving the matrix, 
    ## if the matrix hasn't changed
    if (all.equal(m,y)) {
      message("identical matrix passed as arg")
      return()
    } 
        
    ##simply assign the value passed to the matrix (m)
    m <<- y
    
    ## when matrix (m) is set, the inverted cached matrix becomes invalid,
    ## hence null it to ensure data integrity.
    ## Alternative would be to mInv <- solve(m), but again, to save CPU
    ## solving the matrix should only be done when fGetInversed is called
    mInv <<- NULL 
  }
  
  fGet <- function () {
    # simply return the matrix
    m
  }
  
  fGetInverted <- function () {
    ## if inverted matrix hasn't been cached (i.e. it's currently NULL)
    ## then solve m and cache result in mInv
    if (is.null(mInv)) {
      message("caching inverted matrix")
      mInv <<- solve(m)
    }
    
    ## return the inverted (and cached) matrix
    mInv
  }
    
  ##return a list of exposed functions
  list( set = fSet, 
        get = fGet,
        getInverted = fGetInverted)
}


cacheSolve <- function(m = maxtrix()) {
##----------------------------------------------------------------------------------  
##  Args:
##    m: an atomic matrix OR a special "matrix" returned by makeCacheMatrix(matrix)
##    
##    Remark: Compared to the makeVector example, additional args ( ,...) are left
##            out in this implementation, to simplyfy the code - and still comply with 
##            the scope of the assignment; if additional args were to be accepted, 
##            then these args would need caching as well - and that's outside the scope 
##            of the assignment
##  
##  Returns:
##    Returns the solved (inverted) matrix of m
##    if availabe, a cached version of the inverted m, will be returned 
##    to save processing time by avoiding calls to solve(matrix)
##
##----------------------------------------------------------------------------------  


  ## make sure m isn't atomic, to catch use cases like cacheSolve(matrix(c1:4,2,2)) 
  ## in a real world example, additional checking should be done, to make sure that
  ## the matrix passed as arg actually exposes the get, set and getInversed functions
  if (is.atomic(m)){
    stop("the matrix passed as argument must be recursive, and of 'type' makeCacheMatrix")
  }

  ## return the inverse matrix of m
  ## m$gerInverted() will _always_ return a cached inverse matrix of m if exists, 
  ## otherwise it will inverse, cache and then return. So it'll never return NULL
  ## hence no need to check for that in this scope, as compared to the cacheMean example
  return(m$getInverted())
}


##----------------------------------------------------------------------------------  
##
## TEST CASES
##
##----------------------------------------------------------------------------------  
matrix <- matrix(1:4,2,2)
m1 <- makeCacheMatrix(matrix)
m1Inv <- cacheSolve(m1)     ## expect mInv to be cached and returned (message: caching inverted matrix)
m2Inv <- cacheSolve(m1)     ## expect mInv read from cache (no message of caching)
m3Inv <- solve(matrix)      ## expect solve(matrix) == m1Inv == m2Inv
m1$set(matrix)              ## expext no change in either m or mInv (message: identical matrix passed as arg)
m4Inv <- m1$getInverted()   ## expect m1Inv == m2Inv == m3inv == m4inv
m5Inv <- cacheSolve(matrix) ## expect error message, that matrix isn't of 'type' CacheMatrix
