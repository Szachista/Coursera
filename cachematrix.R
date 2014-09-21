## Function makeCacheMatrix creates structure which holds matrix
## and its inverse, while cacheSolve returns inverse of a matrix
## (if it is not computed, it will calculate it).

## Function mostly based on the example.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setInverse <- function(X) inverse <<- X
	getInverse <- function() inverse
	list(set = set, get = get,
		setInverse = setInverse, getInverse = getInverse)
}


## Function computes inverse of a matrix (if not computed),
## stores it and returns it.

cacheSolve <- function(x, ...) {
	inverse <- x$getInverse()
	if (is.null(inverse))
	{
		data <- x$get()
		x$set(data)
		inverse <- solve(data)
		x$setInverse(inverse)
	}
	inverse
}
