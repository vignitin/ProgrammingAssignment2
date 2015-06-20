## This R program creates 2 functions 'makeCacheMatrix' & 'cacheSolve' 
##that are used to create the inverse of a matrix and cache the results


## The 'makeCacheMatrix' function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The 'cacheSolve' function computes the inverse of the matrix created by the 'makeCacheMatrix' function.
## If the inverse has already been calculated (and the matrix has not changed), this function returns the 
## inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}
