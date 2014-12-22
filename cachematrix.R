## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    
    ## setting the matrix
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    ## getting the matrix
    get <- function() x
    
    ## setting the solve
    getsolve <- function() s
    
    ## getting the solve
    setsolve <- function(solve) s <<- solve
    
    ## defining the structure
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## get current solve
    s <- x$getsolve()
    
    ## if it is not empty, return the value
    if (!is.null(s)) {
        message ("getting cached data")
        return (s)
    }
    
    ## if it is empty, get the matrix.
    data <- x$get()
    
    ## end find the solve
    s <- solve (data, ...)
    
    ## cache the result
    x$setsolve(s)
    s
}
