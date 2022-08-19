## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function makeCacheMatrix creates a special "matrix", containing a list of functions to

## 1. set the matrix
## 2. get the matrix
## 3. set the inverste of the matrix
## 4. get inverste of the matrix

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


## Write a short comment describing this function
## Calculates inverse matrix of cached matrix of the makeCacheMatrix function,
## only if there is not yet an inverse of the matrix calculated/cashed. If
## there is already an inverse, it will get the inverse from the cached data.
## Otherwise it will calculate the inverse of the matrix with 'solve'

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
