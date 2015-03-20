## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly  The below pair of functions cache the inverse of a matrix.
## There are better ways to acheive the below goal, but this has been written for
## for an assignment at coursera.


## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        im <- NULL
        setmatrix <- function(y) {
                x <<- y
                im <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(inversematrix) im <<- inversematrix
        getinverse <- function() im
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
        
        
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.
## Assumption made for this assignment - matrix supplied is always
## invertible. This function will throw an error if a singular matrix is passed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinverse()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        inputmatrix <- x$getmatrix()
        im <- solve(inputmatrix, ...)
        x$setinverse(im)
        im
}
