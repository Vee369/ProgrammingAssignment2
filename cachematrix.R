## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that creates a special "matrix" object that can cache it's inverse for the input.

makeCacheMatrix <- function(x = matrix()) {

  v <- NULL
  set <- function(y){
    x <<- y
    v <<- NULL
  }
  get <- function()x
  setInverse <- function(Inverse) v <<- Inverse
  getInverse <- function() v
  list(set = set , get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve is a function which computes the inverse of the special matrix returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  v <- x$getInverse()
  if(!is.null(v)){
    message("getting inversed result")
    return(v)
  }
  data <- x$get()
  v <- solve(data, ...)
  x$setInverse(v)
  v
}

