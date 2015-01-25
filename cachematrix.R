## The following pair of functions are used to cache the inverse of a given matrix. When we need to 
## calculate the inverse of the same matrix again, the function returns the cached inverse instead 
## of re-computing it.

## The first function creates a special "matrix" object that can cache its inverse.
## 4 functions will be stored under a list in this object.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## The second function returns the cached inverse if we are given the same original matrix, and
## calculates the matrix inverse otherwise.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    ## !is.null ensures that inverse has not already been calculated 
    ## !identical ensures that matrix has not changed
    if(!is.null(m) && !identical(x,x$get())) {
        message("getting cached data")
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setinv(m)
    m
}
