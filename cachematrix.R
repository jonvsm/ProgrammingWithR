#makeCacheMatrix and cachsolve together illustrate some key aspects of lexical scoping in R

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

## cacheSolve calls functions defined in makeCacheMatrix
##It calculates the inverse of the special "matrix" created with the function above. 
##It first checks to see if the inverse has already been calculated and has not changed.
##If so, it gets the inverse matrix from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the matrix and saves the inverse in the cache via the setinverse function.

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