## Functions for inverting a matrix and caching the result.
## For example:
##   m <- matrix(c(4,3,3,2), nrow=2,ncol=2)
##   smo <- makeCacheMatrix(m)
##   cacheSolve(smo)
##   cacheSolve(smo) # should use the cached value

## This function creates a special "matrix" object that can cache its inverse.
## x is the matrix to be 'wrapped'
## returns x wrapped in a special "matrix" object
makeCacheMatrix <- function(mat = matrix()) {
    i <- NULL
    set <- function(y) {
		mat <<- y
		i <<- NULL
    }
    get <- function() mat
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above.
## x is the special matrix object to be inverted.
## returns the inverse matrix of x.
cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	mat <- x$get()
	i <- solve(mat, ...)
	x$setinverse(i)
	i
}

