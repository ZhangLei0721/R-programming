## The following two functions can cache the inverse of a matrix
## example: set x <- matrix(rnorm(16), 4, 4)
## then call cacheSolve(makeCacheMatrix(x)) to get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
##function: Create a special "matrix" object that can cache its inverse
##usage: Pass the result of a makeCacheMatrix function to cacheSolve
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)){
## used to extract the inverse of the matrix if it already exists
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data)
	x$setinverse(m)
	m
## Return a matrix that is the inverse of 'x'
}
