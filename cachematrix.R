## Put comments here that give an overall description of what your
## functions do

## Caches a matrix & its inverse

makeCacheMatrix <- function(m = numeric()) {
        minv <- null
        set <- function(y) {
                m <<- y
                minv <<- NULL
        }
        get <- function() m
        setinv <- function(solve) minv <<- solve
        getinv <- function() minv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Calcs the inverse matrix or uses cached version

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv <- x$getinv()
        if (!is.null(minv)){
                message("Using cached inverse")
        } else {
                message("Need to solve inverse")
                data <- x$get()
                minv <- solve(data,...)
                x$setinv(minv)
        }
        minv
}
