## This function calculates the inverse of a matrix using solve function.
## The inverse matrix is obtained from the cache without calculting it as it is an expensive operation
## Two separate functions makeCacheMatrix and CacheSolve are used to achieve this.



## This function makeCacheMatrix initialises the matrix value, sets & gets the inverse value from cahce using <<- operator
## Inverse of very large matrix is resource intensive operation and performance is better when retrieved from Cache

makeCacheMatrix <- function(x = matrix()) {
  
  m <- matrix(nrow=0, ncol=0)
  set <- function(y) {
    x <<- y
    m <<- matrix(nrow=0, ncol=0)
  }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
 

}


## This function calculates the inverse of matrix using solve if it is readily not available in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()
  ## check the dimensions of matrix is 0 as is.null on matix may not give correct results.
  if(dim(m)[1] != 0) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
