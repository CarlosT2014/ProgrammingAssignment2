## The first function makeCacheMatrix creates a list 
## object with a matrix, prepared to cache the 
## inverse matrix and with the functions set, get and 
## inverse that does all the work.
## The second function ´cacheSolve´ does nothing
## but return x$inverse(), calculating the inverse or
## dwtching the cached inverse matrix

## Function to create an matrix object 
## ready to hold the cached data

makeCacheMatrix <- function(x = matrix()) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- inverse(data, ...)
    x$setinverse(inv)
    inv
}

## Function that checks if the result is cached
## If not it calculates the inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
