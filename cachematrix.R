## Creates a special "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

	## x is a square (invertible) matrix

	i <- NULL
	set <- function(y) {
		
		## the '<<-' operator assigns a value to objects outside the current environment
		
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i

	## return a special 'matrix' object that can cache its inverse

	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}




## Computes the inverse of the special "matrix'.

cacheSolve <- function(x, ...) {

	## x is the output of makeCacheMatrix()

	i <- x$getinverse()

	## check whether the inverse has been made
	
	if(!is.null(i)){
		
		## if TRUE, get it from the cache and skip

		message("getting cached data")
		return(i)
	}

	## if FALSE, calculate the inverse

	data <- x$get()
	i <- solve(data, ...)

	## set the value of the inverse from the cache

	x$setinverse(i)

	## return the value

	i
}


