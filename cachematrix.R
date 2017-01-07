##These functions together illustrate some key aspects of lexical scoping in R

##makeCacheMatrix takes a matrix as input and defines a set of functions - set, 
##get, setinverse, getinverse for acting on that matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}   

## cacheSolve calls functions defined in makeCacheMatrix to check if the inverse
##of the matrix is cached and, if it is not, call getinverse and return the inverse

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