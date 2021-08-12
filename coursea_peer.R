
## Grace L.
## Aug.11/2021
## Create a "matrix" x that can cache its inverse.
## "inv" is the property


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() {
		x
	}
	setInverse <- function(inverse) {
		inv <<- inverse
	}
	getInverse <- function() {
		inv
	}
	list(set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## Return the inverse matrix
cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if (!is.null(inv)) {
		message("getting cached data:")
		return(inv)
	}
	res <- x$get()
	inv <- solve(res, ...)
	x$setInverse(inv)
	inv
}
