## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly 
## The following functions are used to provide the 
## functionality of caching the inverse of a matrix
## Its assumed that the matrix supplied is always invertible


## Creates a special "matrix" object that can cache its inverse
## Returns a list of functions for
## 1. Set the value of the martrix
## 2. Get the value of the matrix
## 3. set the Value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
## If the matrix is not found in the cache it will calculate the inverse, 
## store the results in the cache and return the inverse
cacheSolve <- function(x, ...) {
  
  ## Try to find the data from cache      
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ##Calculate the inverse, store and return
  data <- x$get()
  ##Assumes that the matrix is always invertible
  inv <- solve(data, ...)
  
  x$setinverse(inv)
  inv
}
