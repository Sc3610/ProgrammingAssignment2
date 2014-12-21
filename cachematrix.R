## Put comments here that give an overall description of what your
## functions do

##This function creat the matrix x and assigns the inverse with a null value

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y){
    x <<- y
    invrs <<- NULL
  }
##This function uses x and 'solve' calculates the inverse
  get <- function() x
  setinvrs <- function(solve) invrs <<- solve
  getinvrs <- function() invrs
  list(set = set, get = get, 
       setinvrs = setinvrs, getinvrs = getinvrs)
}

## cachesolve function calculates the inverse and the if function checks to see if the inverse of matrix x has already been calculated then returns the invrs

cacheSolve <- function(x=invrs, ...) {
  invrs <- x$getinvrs()
  if(!is.null(invrs)){
    message("getting cached data")
    return(invrs)
  }
  data <- x$get()
  invrs <- solve(data, ...)
  x$setinvrs(invrs)
  invrs
}
