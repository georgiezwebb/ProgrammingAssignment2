## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a new environment where a matrix object is created 
## and its value can be set and retrieved
makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
		set <- function(y){
			x <<- y
			i <<- NULL
		}
		get <- function() x
		setinverse <- function(solve) i <<- solve
		getinverse <- function() i
		list(set = set, get = get, 
			setinverse = setinverse, 
			getinverse = getinverse)
}


## Write a short comment describing this function
## chacheSolve checks makeCacheMatrix that inverse has not been previously
## calculated. If not the new inverse is calclated and added to the cached object.
cacheSolve <- function(x, ...) {
		i <- x$getinverse()
		if(!is.null(i)){
			message("getting cached data")
		  ##Return previously chached matrix
			return(i)
		}
		data <- x$get()
		i <- solve(data, ...)
		x$setinverse(i)
		##Return new matrix
		return(i)
}