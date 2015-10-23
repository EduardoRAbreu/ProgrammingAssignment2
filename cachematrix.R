## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## Calculates the inverse of the matrix and caches it for later use

makeCacheMatrix <- function(x = matrix()) {
  base <<- x
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() m
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
  }


## Write a short comment describing this function


## Checks to see if matrix has changed, if not, pulls inverse from cache
## If the matrix has changed, it calculates the inverse again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if(x != base){ ## Checks to see if the matrix has changed
    makeCacheMatrix(x) ## If it changed, invokes previous function to calculate it
  }
  
  inv <- x$getinv()
  if(!is.null(inv)){
    message("retrieving cached data")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  inv$setinv(inv)
  inv
}
