## The code below provides two functions that (1) creates a special matrix
## and (2) calculate the inverse of the matrix, but returns the stored
## value if it already exists. 

## The function below will convert a matrix into a Cache Matrix with 4 internal functions:
##     get : get the value of the matrix
##     set : set the value of the matrix
##     getinverse : get the value of the inverse matrix
##     setinverse : set the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates the inverse of the matrix, but first checks to see
## if it already exists and if so returns the stored value

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
