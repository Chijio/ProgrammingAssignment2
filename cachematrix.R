## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## A pair of functions that cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	
	set <- function(matrix) {
		m <<- matrix
		i <<- NULL
	}
	
	get <- function() {
		m
	}
	
	setInverse <- function(inverse) {
		i <<- inverse
	}
	
	getInverse <- function() {
		i
	}
	
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## Write a short comment describing this function

## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cacheSolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getInverse()
		
		if(!is.null(m) ) {
			message("getting cached data")
			return(m)
		}
		
		## Get the matrix from our object
		data <- x$get()
		
		## Calculate the inverse using matrix multiplication
		m <- solve(data) %*% data
		
		## Set the inverse to the object
		x$setInverse(m)
		
		## Return the matrix
		m
}
