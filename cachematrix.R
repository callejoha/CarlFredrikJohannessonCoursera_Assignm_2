## Functions to cache the inverse of a matrix.

## First function produces a special "matrix" object that caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # initialise inv
    inv <- NULL
    
    # define set function for matrix
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    # define get function for matrix
    get <- function() x
    set_inv <- function(solveMatrix) inv <<- solveMatrix
    get_inv <- function() inv
    # assign functions as elements in list
    list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}

# Second fucntion returns the inverse of the matrix
cacheSolve <- function(x, ...) {
    ## Return inverse of X in matrix
    inv <- x$get_inv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$set_inv(inv)
    inv      
}