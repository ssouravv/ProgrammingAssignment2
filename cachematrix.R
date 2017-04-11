## There are two functions. First "makeCacheMatrix" takes a matrix	  ##
## input and returns a list of functions that sets or gets a matrix	  ##
## whose inverse can be cached. The second function "cacheSolve"	  ##
## returns the inverse of the matrix of the format returned by first	  ##
## function. If it cached then it returns cached data without calculating ##
## otherwise, it calculates inverse, caches it and returns inverse	  ##

## This first function makes a matrix which can cache it's inverse	  ##

makeCacheMatrix <- function(x = matrix()) {
	# initially inverse of the matix is not calculated, so set to null
	inverse <- NULL
	# function to set the matrix by the argument passed
	set <- function(y){
		# scope assignment operator used to set the matrix for parent 
		x <<- y
		# inverse is not calculated and hence cached at the beginning
		inverse <<- NULL
	}
	# function to return the main matrix
	get <- function() x
	# function to set the inverse matrix for caching
	setinverse <- function(inv) inverse <<- inv
	# function to return the cached inverse matrix or null
	getinverse <- function() inverse
	# return of parent function a list getters and setters of the matrix 
	# and it's inverse which serves for caching and returning inverse
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse )
}


## This second function returns either cached or calculated inverse of	  ##
## a matrix formed by first function					  ##

cacheSolve <- function(x, ...) {
        # attempting to get the inverse matrix from the arg passed
	inverse <- x$getinverse()
	# checking if inverse in cached, then return the inverse 
	if(!is.null(inverse)){
		message("getting cached data")
		return(inverse)
	}
	# if inverse is not cached then get the matrix
	data <- x$get()
	# inverse is calculated assuming square matrix is passed
	inverse <- solve(data)
	# setting the inverse matrix for caching
	x$setinverse(inverse)
	# returning the inverse matrix of non-cached matrix
	inverse
}
