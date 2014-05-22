## -- Create a fuction makecacheMatrix that takes a matrix as input
## -- and will return the inverse of the matrix in cache
makeCacheMatrix <- function(x = matrix()) {
	## -- set the initial value of the cache to null
	m <- NULL
	## -- pass in the matrix
	set <- function(y) {
		x <<- y
		## -- clear the cache
		m <<- NULL
	}
	## -- get the elements of the matrix
	get <- function() x
	
	## -- create the inverse of the matrix and store in m
	setinverse <- function(solve) m <<- solve
	
	## -- get the inverse of the matrix
	getinverse <- function() m
	
	## return the 4 functions in a list so they can be called
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}

## -- create a function cacheSolve to return the inverse. 
## -- if the matrix is already in cache, return it
## -- otherwise recalculate the inverse
cacheSolve <- function(x, ...) {
	## -- return the inverse of the original matrix		
	m <- x$getinverse()
	
	## -- check to see if the inverse is already cached
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	## -- if not in cache, go ahead and calculate the inverse
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}