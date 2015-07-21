## The functions cache the inverse of an invertible matrix.  
## NOTE: It is assumed that the inputted matrix is invertible - no error checking on this is performed. 


## makeCacheMatrix returns a special matrix object 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Checks if the inverse of the inputted "special" matrix object has already been cached and if it has not it will compute it.  
## Otherwise it returns the cached inverse of the "special" Matrix object and caches it. 
## 
##Inputs:   Special Matrix object returned by the makeCacheMatrix function
##Returns:  Inverse of the inputted matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,... )
  x$setinv(inv)
  inv
  
}
