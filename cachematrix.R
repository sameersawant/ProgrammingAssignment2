## Put comments here that give an overall description of what your
## functions do

## Creates and returns a "matrix" with
## named elements that are functions for
##  i)  getting the value of the matrix 
## ii)  setting the value of the matrix
##iii)  getting the matrix inverse
## iv)  setting the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	
	get <- function() x
	
	setInverse <- function(inv) inverse <<- inv
	
	getInverse <- function() inverse
	
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Calculates the inverse of the "matrix" created 
## with makeCacheMatrix. First, checks if the inverse
## is available in the cache and if so returns the same
## otherwise calculates the inverse and sets it on the
## matrix.

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
		
	if(!is.null(i)) {
		message("Getting cached data.")
		return (i)
	}

	data <- x$get()
	i <- solve(data, ...)
	x$setInverse(i)
	i
}


## code to run test on cacheSolve and makeCacheMatrix
runTest <- function() {
	x <- matrix(c(2,2,3,2),nr=2,nc=2)
	v <- makeCacheMatrix(x)
	print("Input matrix.")
	print(v$get()) # should print the matrix
	print("v$getInverse() should be NULL.", quote=FALSE)
	print(v$getInverse()) # should print NULL
	cacheSolve(v)
	print("Diagnostic message 'Getting cached data.' should get printed.", quote=FALSE)
	cacheSolve(v) # should print "Getting cached data."
	print("Cached matrix inverse.", quote=FALSE)
	print(v$getInverse()) # should print the inverse.
}