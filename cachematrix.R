## Caching the Inverse of a Matrix
##
## Matrix inversion is usually a costly computation, so the two follwing functions
## are used to cache the inverse of a matrix so that it does not to be computed
## repeatedly.


## The function makeCacheMatrix creates a list 
## object with a matrix, prepared to cache the 
## inverse matrix and with the following functions
## (that do all the work):
## 1. set           set the value of the matrix
## 2. get           get the value of the matrix
## 3. setinverse    set the value of inverse of the matrix
## 4. getinverse    get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The second function ´cacheSolve´ returns
## It checks if the result is cached (it has been
## already computed). 
## If so, it returns the cached inverse matrix; if not
## it computes it

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
