## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#   How to test the matrix
#   x=matrix(data = 1:4, nrow=2, ncol=2);
#   x_cache <- makeCacheMatrix(x); x_inv<- cacheSolve( x_cache ); x_inv
#   Compare with 
#   x_inv_correct <- solve(x);x_inv_correct
#   Run again 
#   x_inv<- cacheSolve( x_cache ); x_inv
#   You should be able to see "getting cached data"

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
       getinverse = getinverse )
  
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
  
}
