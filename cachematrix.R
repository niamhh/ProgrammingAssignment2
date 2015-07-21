## The functions cache the inverse of an invertible matrix.  
## NOTE: It is assumed that the inputted matrix is invertible - no error checking on this is performed. 
## Example usage:
## > z<-matrix(c(1,-1,1,2),2,2)               // Create an invertible matrix z
## > z1 <- makeCacheMatrix(z)                 // Create our special matrix
## > z1$get()                                 // Return the matrix
## > cacheSolve(z1)                           // Return the inverse
## > cacheSolve(z1)                           // Call the 2nd time, so return
##
##
## makeCacheMatrix returns a list of four functions.
## 1. set: defines a function to set the argument matrix (x) to a new matrix (y) and sets the inverse (inv) to NULL
## 2. get: Returns the matrix (x)
## 3. setinv: takes an argument and sets the inverse (inv) to this value 
## 4. getinv: returns the inverse (inv)

makeCacheMatrix <- function(x = matrix()) {
    #inv will store the cached inverse of x 
    inv <- NULL
  
    #set the value of x and set the inv varable to NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
  
    #return the value of x
    get <- function() x
  
    #set the value of inv
    setinv <- function(inverse) inv <<- inverse
  
    #return the value of inv
    getinv <- function() inv
  
    #the function returns a list of the above functions 
    list(set = set, 
         get = get, 
        setinv = setinv, 
        getinv = getinv)
}


## Checks if the inverse of the inputted special matrix object has already been cached and if it has not it will compute and cache it.  
## Otherwise it returns the cached inverse of the "special" Matrix object 
## 
##Arguments:   Special Matrix object returned by the makeCacheMatrix function
##Returns:    Inverse of the inputted matrix 

cacheSolve <- function(x, ...) {
  
    inv <- x$getinv()
  
    # If the inverse is not NULL return it
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
  
    # Otherwise get the value of the base matrix 
    data <- x$get()
  
    # and calulate its inverse
    inv <- solve(data,... )
  
    # Cache the inverse
    x$setinv(inv)
  
    #return it 
    inv
  
}
