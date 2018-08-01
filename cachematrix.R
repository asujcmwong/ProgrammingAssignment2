## This function creates a special "matrix" object that can cache its inverse.
## functions do

## Write a short comment describing this function
 
 makeCacheMatrix <- function(x = matrix()) {
   
   ## initialize variable
     inv <- NULL
     
     set <- function(y) {
         x <<- y
         inv <<- NULL
     }
     get <- function() x
     ## computes the inverse of non-singular matrix via the solve function
     setinverse <- function(solve) inv <<- solve
     getinverse <- function() inv
     
     ## passes the value of this function 
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
 


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been  
## calculated(and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
  
    ## return inverse(cached data) if exists
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    
    ## compute
    data <- x$get()
    inv <- solve(data, ...)
    
    x$setinverse(inv)
    
    ##output result
    inv
}
