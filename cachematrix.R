## The following 2 functions caches and computes the inverse
## of a given matrix

## This function creates a special object that can create
## a matrix and store its inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <-function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) inverse <<- inv
	getinverse <- function() inverse
	list(set = set, get = get,
		setinverse = setinverse, 
		getinverse = getinverse)
}


## This function retrieves the inverse of a matrix if it 
## was cached or computes it if it was not cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
	if(!is.null(inverse)) {
		message("Getting cached data ...")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data, ...)
	x$setinverse(inverse)
	return(inverse)
}
}
