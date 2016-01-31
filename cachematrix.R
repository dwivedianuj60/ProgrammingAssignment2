## we are going to create two functions.

## first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("receiving cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

## below is the demostration of above sampples.
x = rbind(c(4,6,8), c(10,12,16), c(2,4,8))
m = makeCacheMatrix(x)
m$get()
[,1] [,2] [,3]
[1,]    4    6    8
[2,]   10   12   16
[3,]    2    4    8
> cacheSolve(m)
[,1]   [,2]          [,3]
[1,] -1.0  0.500  3.552714e-16
[2,]  1.5 -0.500 -5.000000e-01
[3,] -0.5  0.125  3.750000e-01
> 
