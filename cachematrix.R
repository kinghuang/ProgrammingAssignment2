## cachematrix contains two utility functions for solving the inverse
## of matrices and stored the results for reuse.

## makeCacheMatrix takes in a matrix x and returns a list containing
## functions for getting and setting the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(minv) inv <<- minv
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve takes in a cache matrix x and returns the matrix's
## inverse. x is a list created by the makeCacheMatrix function.
## If x has a previously calculated inverse, it is used as the
## return value. Otherwise, the inverse is calculated, stored
## in x, and returned.

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
