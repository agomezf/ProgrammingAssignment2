## This functions calculate and cache the inverse of a matrix.

## This function receives a matrix as an argument and returns a list with 4 
## functions: set, get, setsolve, getsolve.
## The get and set functions set and return the value of the matrix.
## The setsolve and getsolve functiones set and return the inverse of the 
## matrix.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inverse <<- solve
        getsolve <- function() inverse
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The function returns the inverse of a matrix by passing the makeCacheMatrix 
## as an argument. If the inverse of matrix is already calculated, this value 
## will be returned. If not, first the inverse will be calculated and chached.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getsolve()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setsolve(inverse)
        inverse
}
