## Contains pair of functions that cache the inverse of a matrix
## makeacheMatrix accepts a square matrix and cache the inverse of the passed matrix
## d <- makeCacheMatrix(matrix(c(1,2,3,4),2,2))
##> cacheSolve(d)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> d$set(matrix(c(4,5,6,7),2,2))
##> cacheSolve(d)
##[,1] [,2]
##[1,] -3.5    3
##[2,]  2.5   -2
##> cacheSolve(d)
##getting cached data
##[,1] [,2]
##[1,] -3.5    3
##[2,]  2.5   -2

makeCacheMatrix <- function(x = matrix()) {
  
   invmatrix <- NULL
  
  ## change resets invmatrix to null 
  set <- function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(invm) {
    invmatrix <<- invm
  }
  getinverse <- function() invmatrix
  
  list(set = set , get = get, setinverse = setinverse , getinverse = getinverse)
  

}


## cacheSolve matrix inverses a matrix if it is not already inversed or changed and return inversed matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'  
  invm <- x$getinverse()
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data, ...)
  x$setinverse(invm)
  invm
  
}
