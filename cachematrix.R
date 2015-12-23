## The two functions written below are used to cache the inverse of a matrix.
##
## The first function, makeCacheMatrix creates a list containing functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
##
## x in the function is the square invertible matrix

makeCacheMatrix <- function(x = matrix()) {
  
  matinv <- NULL
  set <- function(y) {
    x <<- y           
    matinv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matinv <<- inverse
  getinverse <- function() matinv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}

## The cacheSolve function calculates the inverse of the matrix created with the
## above function. However, it first checks if the inverse has already been computed.  
## If so, it gets the inverse from the cache and skips the calculation. If not, then 
## it calculates the inverse of the matrix and sets the value of the inverse in the 
## cache via the setinverse function.

cacheSolve <- function(x, ...) {
  
  matinv <- x$getinverse()
  if(!is.null(matinv)) {
    message("getting cached data.")
    return(matinv)
  }
  data <- x$get()
  matinv <- solve(data)
  x$setinverse(matinv)
  matinv                        ## Returns a matrix that is the inverse of 'x'

}
