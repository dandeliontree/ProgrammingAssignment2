##This pair of functions inverts a matrix (x), after the
##inverse is calculated, for the first time, the result is 
##cached. 
##
##The makeCacheMatrix creates smartMatrix that can cache its 
##inverse.
##
##The cached result is returned for subsequent calls to
##the cacheSolve function provided it is passed the same 
##smartMatrix. 
## 
##NOTE: the matrix MUST BE INVERTIBLE.



## Factory function that takes an invertible matrix x and 
## creates a smartMatrix with the ability to cache its 
## inverse.
##
## The smartMatrix matrix is returned as a list of 
## functions that invert x and assign the result to
## cachedInverse.
##  
## 
makeCacheMatrix <- function(x = matrix()) {
	##Inverse Cache
	cachedInverse <-NULL;
	
	##Set the matrix to be inverted
	##Invalidate the cached result.
	set <-function(y) {
		x<<-y
		cachedInverse <<-NULL
	}
	get<-function( )x

	setInverse<-function(inverse) cachedInverse <<-inverse
	getInverse<-function() cachedInverse
	list(set = set, get = get,
           setInverse= setInverse,
           getInverse= getInverse)
}


## cacheSolve returns the inverse of smartMatrix.
##
## If the the function has not been invoked for 
## smartMatrix the inverse is calculated, cached, and then 
## returned to the user. If the function has been called for 
## smartMatrix the inverse is read from the cache
## and the cached invese is returned to the user.
##
##
cacheSolve <- function(smartMatrix, ...) {
	
	inverse<-smartMatrix$getInverse()
	if(!is.null(inverse)){ ## second or subsequent call use cached value
		message("using smartMatrix's cached inverse")
		return(inverse)
	}
	
	##Firts call fetch the data from the 'smartMatrix'
	matrix2Invert<-smartMatrix$get()

	##Invert the Matrix
	inverse<-solve(matrix2Invert)

	##Cache the inverted matrix
	smartMatrix$setInverse(inverse)
	
	## Return the inverse of 'smartMatrix'
	inverse	   
        
}
