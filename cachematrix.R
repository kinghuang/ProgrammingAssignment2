## cachematrix contains two utility functions for solving the inverse
## of matrices and stored the results for reuse.

## makeCacheMatrix takes in a matrix x and returns a list containing
## functions for getting and setting the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
	# Initialize the inverse of x to NULL.
	inv <- NULL
	
	# get and set functions for the matrix x.
	# The set function also sets the inverse of x
	# to NULL, to invalidate any previously cached
	# inverse.
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	
	# get and set functions for the inverse of
	# the matrix x.
	setinv <- function(minv) inv <<- minv
	getinv <- function() inv
	
	# Return a simple object (list) containing the
	# get and set functions for a matrix and its
	# inverse.
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve takes in a cache matrix x and returns the matrix's
## inverse. x is a list created by the makeCacheMatrix function.
## If x has a previously calculated inverse, it is used as the
## return value. Otherwise, the inverse is calculated, stored
## in x, and returned.

cacheSolve <- function(x, ...) {
	# Get the cached inverse from x. If the
	# returned value is not null, return
	# that as the inverse of the matrix
	# stored in x.
	inv <- x$getinv()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	
	# Otherwise, get the matrix in x, and
	# solve for its inverse. The result is
	# cached in x for subsequent to cacheSolve,
	# for this matrix, and returned.
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
