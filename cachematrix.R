## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function:



## makeCacheMatrix: creates a special "matrix" object that could cache its inverse
## Creating a makeCacheMatrix object will consist of four functions encapsulated in a list
## { 1. set the matrix, 2. get the matrix, 3. set the inverse of the matrix, 4. get the inverse of the matrix }
makeCacheMatrix <- function(x = matrix()) {

	# Initially set to NULL , it changes when the user sets the value
	invMatrix <- NULL
	
	# Set function. Sets the matrix itself but not the inverse
	set <- function(y) {
		x <<- y
		invMatrix <<- NULL
	}

	# Get function. Gets the matrix itself but not the inverse
	get <- function() x

	# Manually set the inverse
	setInvMatrix <- function(inverse) invMatrix <<- inverse

	# Get the inverse
	getInvMatrix <- function() invMatrix
	
	# Encapsulate into a list
	list(set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}


## Write a short comment describing this function

## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix defined above
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	# Get the current state of the inverse and see if it has been computed yet
	invMatrix <- x$getInvMatrix()
	if(!is.null(invMatrix)) {
		# Simply return the computed inverse
		message("### Getting cached data ###")
		return(invMatrix)
	}

	# Get the matrix itself
	data <- x$get()
	# Find the inverse
	invMatrix <- solve(data)
	# Cache result in the object
	x$setInvMatrix(invMatrix)

	# Return new result
	invMatrix
}
