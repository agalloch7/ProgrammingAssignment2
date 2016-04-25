## This function creates a special "matrix" object that can cache its 
## m.

makeCacheMatrix <- function(x = matrix()) {
	##define the cache
	m <- NULL
	set <- function(x) {
		mtx <<- x; ##assign input matrix x to variable mtx
		m <<- NULL; ##reinitialize m to null
	}
	get <- function() return(mtx); ##return the matrix
	setinv <- function(inv) m <<- inv; ##set m to inverse of matrix x
	getinv <- function() return(m); ##return cached inverse of x
	return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## This function computes the m of the special "matrix" returned
## by makeCacheMatrix above. If the m has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the m from the cache.

cacheSolve <- function(x, ...) {
	m <- mtx$getinv()
	if(!is.null(inserver)){
		message("Getting cached data")
		return(m)
	}
	data <- mtx$get()
	m <- solve(data, ...)
	mtx$setinve(m)
	## Return a matrix that is the m of 'x'
	return(m)
}
