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
## use of `<<-` to assign a value to an object in an environment 
## which is different from the current environment
    
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
  
## To check if the inverse has already been calculated
  
  if(!is.null(matinv)) {
    message("getting cached data.")
    return(matinv)
  }
  
## else, calculate the inverse
  data <- x$get()
  matinv <- solve(data)
  
## To set the value of the inverse in the cache via the setinv function.
  x$setinverse(matinv)
  
## Return a matrix that is the inverse of 'x'
  matinv                        

}

## Testing the functions :
## > x = matrix(1:4, nrow=2, ncol=2)
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]    1    3
## [2,]    2    4

## First run without the cache
## > cacheSolve(m)
##       [,1]  [,2]
## [1,]   -2    1.5
## [2,]    1   -0.5

## Second run when function retrives from the cache
## > cacheSolve(m)
## getting cached data.
##       [,1]  [,2]
## [1,]   -2    1.5
## [2,]    1   -0.5
## 
