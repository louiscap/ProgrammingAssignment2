## These functions allow the user to create matrix type object
## which can cache its inverse. When the user wants to find the
## inverse of the matrix, if a cached version is available that
## will be returned. If not, then the inverse will be calculated.

## This function creates a matrix type object.
## Once created, the matrix can only be accessed using "set" and "get"
## The inverse can be stored in the "inv" variable,
## but can only be accessed using "setinverse" and "getinverse"

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ## this will hold the "cached" inverse of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(inverse) {
        inv <<- inverse
    }
    getinverse <- function() {
        inv
    }
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse) ## this list is returned
}


## Find the inverse of the matrix. First look for cached version.
## If not available, calculate inverse using "solve()"

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("Getting cached inverse")
        return(inv) ## return cached inverse and exit this function
    } else {
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv) ## put calculated inverse into the cache
    }
    inv ## return the calculated inverse
}
