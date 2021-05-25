## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y) {
      x <<- y
      inver <<- NULL
    }
    get <- function() x
    setinver <- function(inverse) inver <<- inverse
    getinver <- function() inver
    list(set = set, get = get, setinver = setinver, getinver = getinver)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  inver <- x$getinver()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  matrixtosolve <- x$get()
  inver <- solve(matrixtosolve, ...)
  x$setinver(inver)
  inver
  ## Return a matrix that is the inverse of 'x'
}
