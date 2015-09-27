## Matrix inversion is usually a costly computation and there may
## be some benefit to caching the inverse of a matrix rather than
## computing it repeatedly. The functions below create a matrix and
## cache its inverse.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache. It assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
	inverse <- x$getInverse()
	if(!is.null(inverse)) {
		message('getting cached matrix')
		return(inverse)
	}
	matrix <- x$get()
	inverse <- solve(matrix)
	x$setInverse(inverse)
	inverse
}
