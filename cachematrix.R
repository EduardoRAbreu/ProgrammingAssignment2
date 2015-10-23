## These functions are designed to calculate the inverse of a
## matrix and cache the output


## makeCacheMatrix calculates the inverse of the matrix and caches
## it for later use

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, 
       get = get, 
       setinv = setinv, 
       getinv = getinv)
  }


## cacheSolve checks to see if inverse of matrix exists. 
## If not, it calculates the inverse

cacheSolve <- function(x, ...) {
          
  inv <- x$getinv()
  if(!is.null(inv)){
    message("retrieving cached data")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

