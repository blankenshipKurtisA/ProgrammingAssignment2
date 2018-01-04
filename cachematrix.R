## Two functions to create a variable to store a matrix and its cached inverse,
## clearing the cache when the matrix is updated.

##Create a variable with (4) sub functions:
##  get():  get the matrix value
##  set(x):  set matrix value to x and clear cached inverse
##  getinverse():  get the matrixes inverse (NULL if cacheSolve has not been run)
##  setinverse():  set the inverse to a specific value

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Pass a makeCacheMatrix to the function to calculate and set the cached inverse
## of the matrix if it is NULL, and return the inverse of the matrix

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
	if(!is.null(inv)) {
		return(inv)
	}
	mat <- x$get()
	inv <- solve(mat,...)
	x$setinverse(inv)
	inv
}
