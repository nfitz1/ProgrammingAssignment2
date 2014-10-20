## These functions create a matrix whose inverse can be cached, and compute and cache that inverse.
## If the inverse has already been cached, the functions return the value that was cached 
##instead of re-computing the inverse.


## Write a short comment describing this function
##makeCacheMatrix creates a matrix and functions to define the matrix, get the original matrix
## set the inverse of the matrix, and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y  
    m <<- NULL
  }
  get <- function() x  # works
  setinverse <- function(solve) m <<- solve ##
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
      message("getting cached data")
      return(m)
    }
    matrix <- x$get()
    m <-solve(matrix)
    x$setinverse(m)
    m
  }
  

