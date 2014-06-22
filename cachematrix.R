## This is for R Programming section 004 (Prof. Peng) assignment number two. 
## It includes functions called makeCacheMatrix and cacheSolve 

## This function should create a special matrix object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
 	inverse <- NULL 
	set <- function(y) {
			x <<- y 
			inverse <<- NULL 
		}
	get <- function() x 
	setinverse <- x%*%x  <<- inverse
	getinverse <- function () inverse
	list (set = set, get = get, setinverse = setinverse, 		
		getinverse = getinverse)  
}

## This function computes the inverse of the special matrix 
## returned by makeCacheMatrix above.  If the inverse has already
## been calculated, and the matrix has not changed, then 
## the cacheSolve function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message(“getting cached data”)
		return (m)
	}
	data <- x$get() 
	m <- inverse(data, ...) 
	x$setinverse(m)
	m
}
