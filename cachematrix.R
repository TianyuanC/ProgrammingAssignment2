## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    xInverse <- NULL
    set <- function(y) {
        x <<- y
        xInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) xInverse <<- inverse
    getInverse <- function() xInverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}

## Test script
#data<-matrix(c(1,2,3,4),2,2)
#data<-matrix(rnorm(10000),100,100)
#cm<-makeCacheMatrix(data)
#rslt<-cacheSolve(cm)
#rslt<-cacheSolve(cm) #show "getting cached data" on the second run