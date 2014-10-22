## These functions create a matrix whose inverse can be cached, and compute and cache that inverse.
## If the inverse has already been cached, the functions return the cached value and do not re-compute the inverse
## Both functions are based on the example functions provided in the "Caching the Mean of a Vector" example 


##makeCacheMatrix creates a matrix and the functions which:
##define the original matrix, get the original matrix, set the inverse of the matrix, and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y  
    m <<- NULL
  }
  get <- function() x  
  setinverse <- function(solve) m <<- solve 
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  




## This function computes the inverse of the matrix returned by makeCacheMatrix, and caches it in setinverse.
## If the inverse has already been calculated and the matrix has not changed, cacheSolve retrieves
## the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    m <- x$getinverse()
    if(!is.null(m)) {  
      message("getting cached inverse")
      return(m)
    }
    matrix <- x$get()
    m <-solve(matrix)
    x$setinverse(m)
    m
  }
  

