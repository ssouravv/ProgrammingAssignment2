## There are two functions. First "makeCacheMatrix" takes a matrix
## input and returns a list of functions that sets or gets a matrix
## whose inverse can be cached. The second function "cacheSolve"
## returns the inverse of the matrix of the format returned by first
## function. If it cached then it returns cached data without calculating
## otherwise, it calculates inverse, caches it and returns inverse

## This first function makes a matrix which can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y){
		x <<- y
		inverse <<- NULL
	}

	get <- function() x
	setinverse <- function(inv) inverse <<- inv
	getinverse <- function() inverse
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse )
}


## This second function returns either cached or calculated inverse of
## a matrix formed by first function

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
	if(!is.null(inverse)){
		message("getting cached data")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data)
	x$setinverse(inverse)
	inverse
}
