## cachematrix.R------------------------------------------------------------------------------------------------| 
## include 2 main functions:                                                                                    |
##   1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.            |
##   2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above|
##--------------------------------------------------------------------------------------------------------------|

## makeCacheMatrix ---------------------------------------------------------------------------------------------|  
##   _ makeCacheMatrix(x = matrix()) - is for creating a "cacheable insverse matrix" from a standard matrix "x" |
##   _ The same approach as makeVector sample. makeCacheMatrix include: 1 local variable and 4 local functions  |
##       * im (short of "inverse matrix") - A local variable will be returned by solve of input matrix "x"      |
##       * set - function to assign new matrix values to "x" matrix                                             |
##--------------------------------------------------------------------------------------------------------------|

makeCacheMatrix <- function(x = matrix()) {
  ## im (short of "inverse matrix") - A local variable will be returned by solve of the input matrix "x"
  ##                                  Initial value of "im" is NULL
  im <- NULL
  
  ## set - function to assign new matrix "y" to this obect's matrix "x"
  ##       "im" will be reset to NULL accordingly
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  
  ## get - function to get object's matrix "x"
  get <- function() x
  
  ## setsolve - function to set "solve" matrix to inverse matrix "im"
  setsolve <- function(solve) im <<- solve
  
  ## getsolve - function to get inverse matrix "im"
  getsolve <- function() im
  
  ## Return all local functions to object created by makeCacheMatrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve --------------------------------------------------------------------------------------------------|  
##   _ cacheSolve(x,...) - is for calculating inverse matrix from special object "x" created by makeCacheMatrix |
##   _ The same approach as cachemean sample:                                                                   |
##       * If the inverse has already been calculated (and the matrix has not changed), then the cachesolve     |
##         should retrieve the inverse from the cache.                                                          |
##       * If the inverse matrix was not calculated, then the inverse of a square matrix can be done with the   |
##         solve() function in R                                                                                |
##--------------------------------------------------------------------------------------------------------------|

cacheSolve <- function(x, ...) {
 
  ## Return a solve matrix of special object "x" to local variable " im" (im: short of inverse matrix)
  im <- x$getsolve()
  
  ## if special object "x" was already calculated for the inverse matrix (mean im != NULL) 
  ## then return the cached inverse matrix "im" to the "cacheSolve" function
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  
  ## if special object "x" was not calculated for the inverse matrix (mean im == NULL) 
  ## then 
  ##   _ get matrix value of special object "x" assign to "data" variable as a matrix
  ##   _ calculate inverse matrix of data by using solve() function
  data <- x$get()
  im <- solve(data, ...)
  
  ## set inverse matrix to special object x
  x$setsolve(im)
  
  ## return inverse matrix to "cacheSolve" function 
  im
}
