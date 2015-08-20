## The goal is to be able to cache any matrix inverse calculations
## E.g.
##Give a matrix ->
##tm <- stats::rnorm(16)
##tm
##dim(tm) <- c(4,4)
##
##Define the CacheMatrix ->
##qq <- makeCacheMatrix(tm)
##
##Load the matrix with the inverse (solve) ->
##cacheSolve(qq)
##

## makeCacheMatrix -> This function creates a special "matrix" object that can cache its inverse.
## It has 4 functions 
## 1.-set -> set the value of the matrix
## 2.-get -> get the value of the matrix
## 3.-setsolve -> set the inverse value
## 4.-getsolve -> get the inverse value

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cache data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

