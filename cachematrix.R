

## makeCacheMatrix creates a special "matrix"	

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL																					##Inverse matrix variable i
	
	set <- function(y) {																		##set the value of the matrix
	x <<- y
	i <<- NULL
	}
	
	get <- function() x																			##get the value of the matrix
	setinverse <- function(inverse) i <<- inverse												##set the value of the inverse
	getinverse <- function() i																	##get the value of the inverse
	list ( set = set, get = get, setinverse = setinverse, getinverse = getinverse )
	
}


## The following function calculates the inverse of the special "matrix" created with the above function. However, it first checks to see if the inverse has already been calculated. ##If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the ##setinverse function.

cacheSolve <- function(x, ...) {

	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")															##Check if the inverse is already present in the Cache	
		return(i)
		}
		
	data <- x$get()
	i <- solve(data, ...)																		##solve is the function to calculate the inverse of a matrix
	x$setinverse(i)
	i
        
}
