## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#   This function takes a matrix as an argument and returns a special vector
#   containing list of functions to the cached object
makeCacheMatrix <- function(x = matrix()) {
  xirtam <- NULL
  set <- function(y) {
    x <<- y
    xirtam <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) xirtam <<- inverse
  
  getInverse <- function() xirtam
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
#  This function takes a cached matrix as an argument and returns the inverse of that matrix.
#	  If the inverse of the matrix has already been solved and stored the cached version 
#   will be returned otherwise it will be resolved and returned.
cacheSolve <- function(x, ...) {
  xirtam <- x$getInverse()
  if(!is.null(xirtam)) {
    message("getting cached data")
    return(xirtam)		# Return cached matrix that is the inverse of 'x'
  }
  data <- x$get()
  if (det(data) == 0) {
    message("Determinant of matrix is 0 therefore no inverse of the matrix exists.")
  }
  else {
    xirtam <- solve(data, ...)
    x$setInverse(xirtam)  
    xirtam			# Return a matrix that is the inverse of 'x'
  }
}