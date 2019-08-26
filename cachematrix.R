## makeCacheMatrix create a cache for matrix x and its inverse matrix

## makeCacheMatrix returns a list containing functions: set(), get(), 
## setInverse() and getInverse() to set and access the values of the 
## matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inv_target) inv <<- inv_target
	getInverse <- function() inv
	list(set = set, get = get, 
		setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve requires an input argument x whose type is a list 
## returned by makeCacheMatrix. the function first checks whether the
## matrix inverse is calculated. if so, it returns the cached results
## or it will calculate and save the results in the cache

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()    ## Return a matrix that is the inverse of 'x'
    if(!is.null(inv)) {
    	message('getting cached data')
    	return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    x$setInverse(inv)
    inv
}
