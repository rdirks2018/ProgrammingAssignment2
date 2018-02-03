
## makeCacheMatrix creates a special matrix object that can cache the inverse of itself.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
		set <- function(y) {
			x <<- y
			inv <<- NULL
			
		}
		get <- function() x
		setInverse <- function(inverse) inv <<- inverse
		getInverse <- function() inv
		list(set = set,
			get = get,
			setInverse = setInverse,
			getInverse = getInverse)
}


## This function calculates the inverse of the special matrix created in makeCacheMatrix.

## If the inverse of the matrix has already been calculated, then the function will retrieve the cached inverse from part 1.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv = x$getinv()
	
	## if the inverse has already been calculated, return cache data
	if (!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
    mat.data = x$get()
    inv <- solve(mat, ...)
   
   ## sets the value of the inverse in cache
    x$setInverse(inv)
    
    return(inv)
        
}
