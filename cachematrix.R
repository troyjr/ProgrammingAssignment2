## 
## Set of functions which create a matrix cacheing object to cache results of
## inverting a matrix - solve(x) for computationally long matrix operations
##
## Usage:
## > # Create square matrix that you want to solve
## > n = 4
## > squarematrix <- matrix(rnorm(n*n), n, n)
## > # create caching matrix object
## > x <- makeCacheMatrix(squarematrix)
## > # calculate inverted matrix
## > View(cacheSolve(x))
## > # running again, it fetches results from cache
## > View(cacheSolve(x))
## getting cached data
## >

## Sets up caching object.
## first argument is square matrix for caching operations

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        
        list(set = set, get = get, setsolve=setsolve, getsolve=getsolve)
}


## Solves and caches square matrix inversion
## subsequent runs are cached
## first argument is object as initialised from 'makeCacheMatrix' function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
