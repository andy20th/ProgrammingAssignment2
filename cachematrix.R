## This function creates a special "matrix" object that can cache its inverse
## Usage: makeCacheMatrix(x)
##        cacheSolve(x)

## prepare the matrix, takes a square matrix as input and outpust a list

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(mean) m <<- mean
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## This function computes the inverse of the  "matrix" or returns it from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m) # retuns inverse from cache (if available)
        }
        data <- x$get()
        m <- solve(data, ...) # computation of inverse
        x$setinv(m)
        m
}