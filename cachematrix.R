## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix 
## rather than computing it repeatedly (there are also 
## alternatives to matrix inversion that we will not discuss here). 
## The following is a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	
	cashe <- NULL
	
	## set matrix
	setM <- function (NewMatrix) {
		x <<- NewMatrix
		cashe <<- NULL
	}
	
	## get matrix
	getM <- function() x
	
	## set matrix inverse
	setMI <- function(solve) cashe <<- solve
	
	## get matrix inverse
	getMI <- function() cashe
	
	list(setM=setM, getM=getM, setMI=setMI, getMI=getMI)
	
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
		
		inv <- x$getMI()
		
        if(!is.null(inv)) {
			message("getting cached data")
			return(inv)
        }
		
        data <- x$getM()
		
        inv <- solve(data)
		
        x$setMI(inv)
		
        inv
}
