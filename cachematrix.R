## These two functions are used to create an object that stores an invertable matrix
## and cache its inverse.




## Creates an object containing a list of getter/setter functions for a 
## matrix and its inverse. mtx represents the stored matrix and inv represents the 
## stored inverse.

makeCacheMatrix <- function(mtx = matrix()) {
    inv <- NULL
    set <- function(y) {
        mtx <<- y
        inv <<- NULL
    }
    get <- function() mtx
    setinverse <- function(m) inv <<- m
    getinverse <- function() inv
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)   
}


## Accepts the object and uses it to solve and cache the inverse of its stored 
## matrix. If an inverse for the matrix already exists, it is simply returned.

cacheSolve <- function(mtx, ...) {
    ## Return a matrix that is the inverse of 'mtx$get()'
    
    inv <- mtx$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        
        return(inv)
    }
    data <- mtx$get()
    inv <- solve(data, ...)
    mtx$setinverse(inv)
    
    inv
}
