## Function works very similar like functions described 
## in assignment for caching the mean of a vector

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1. set - set the value of the matrix
## 2. get - get the value of the matrix
## 3. setInverse - set the inversion of the matrix
## 4. getInverse - get the inversion of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function calculates the inversion of the special "matrix" object 
## created by the "makeCacheMatrix" function

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
