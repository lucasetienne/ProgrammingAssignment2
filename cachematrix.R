## These functions calculate and cache the inverse of a matrix object, and can retrieve the cached inverse

## makeCacheMatrix creates a "matrix" object that is able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL             #assigns "NULL" value to the variable "inv" in the local environment
    set <- function(y) {    #creates "set" function that caches an input variable as "x" and assigns "NULL" value to the variable "inv" in the global environment 
        x <<- y
        inv <<- NULL
    }
    get <- function() x     #creates "get", a function that retrieves the value for "x"
    setinv <- function(solve) inv <<- solve #creates "setinv", a function that sets the inverse by assigning it to "inv in the global environment"
    getinv <- function() inv #creates "getinv", a function that retrieves the inverse matrix (variable "inv")
    list(set = set, get = get, #creates a list of the functions "set", "get", "setinv" and "getinv"
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve retrieves the calculated inverse matrix of the input matrix object "x" or calculates it if it hasn't already been calculated.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()       #assigns the the inverse matrix (value of "inv") to "inv"
    if(!is.null(inv)) {     #checks whether the value of "inv" is still NULL. If not NULL, the value of "inv" is returned.
        message("getting cached data")
        return(inv)
    } else {                    #if there is no cached value...
        data <- x$get()         #assings the input variable to "data"
        inv <- solve(data, ...) #creates the inverse matrix for "data" and assigns it to the variable "inv"
        x$setinv(inv)           #caches the variable "inv" in the local environment to "inv" in the global environment
        inv                     #returns the value of "inv"
    }
}
