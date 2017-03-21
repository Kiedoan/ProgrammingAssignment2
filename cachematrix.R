## Put comments here that give an overall description of what your
## functions do
## The functions below will create a framework to allow for the caching of the inverse of a matrix.

## Write a short comment describing this function
## makeCacheMatrix creates the set, get, setinverse, and getinverse functions to be used in cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function
## cacheSolve checks if the inverse of a matrix has been cached. If so, it returns that inverse. If not, it solves for and returns the matrix inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}
