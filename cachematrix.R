## The functions below are used to create a special "matrix" object,
## and to compute the inverse, retrieving these values from cache whenever available

## makeCacheMatrix returns a list of functions to the environment.
## These functions are used by cacheSolve for matrix calculation.

makeCacheMatrix <- function(x = matrix()) {
    
    #'cache' object stores the calculated values
    cache <- NULL
    
    # create a new matrix, clear cache
    set <- function(value){
        x <<- value
        cache <<- NULL
    }
    
    # returns the matrix
    get <- function() x
    
    # invert matrix, store in cache
    setinverse <- function(inverse) cache <<- inverse
    
    # retrive the inverted matrix from cache
    getinverse <- function() cache
    
    # return list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" object returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then 
## the values are retrieved from the cache.

cacheSolve <- function(x, ...) {
    
    # try to get the value from cache
    inverse <- x$getinverse()
    
    # if there is a cached value avaialable, we will use it
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    
    # if there is no cached value available, we will calculate it now then cache it
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    
    # return the inverse
    inverse
}
