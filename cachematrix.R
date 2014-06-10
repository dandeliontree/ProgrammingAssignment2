##This pair of functions inverts a matrix 'inputMatrix', after the
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



## Factory function that takes an INVERTIBLE matrix 'inputMatrix' 
## and creates a smartMatrix with the ability to cache its 
## inverse.
##
## The smartMatrix matrix is returned as a list of 
## functions that:
##	 i)  Overwrite the original 'inputMatrix' with a new 
##	     value.
##	ii)  Return the matrix to be inverted 'inputMatrix'.
##	iii) Write the inverse to the cache.	
##	 iv) Read the inverse form the cache.
##  
## 
makeCacheMatrix <- function(inputMatrix = matrix()) {
	##Inverse Cache
	cachedInverse <-NULL;
	
	##Overwrite the matrix to be inverted (inputMatrix)
	##with a new matrix to be inverted (newInputMatrix 
	##Invalidate the cached result.
	set <-function(newInputMatrix) {
		inputMatrix <<-newInputMatrix
		cachedInverse <<-NULL
	}

	##Return the matrix for inversion.
	get<-function(){
		inputMatrix
	} 
	
	##Persist the invers to the cache
	setInverse<-function(inverse){ 
		cachedInverse <<-inverse
	}
	
	##REad the inverse from the cache.
	getInverse<-function(){
		cachedInverse
	}
	
	##Create the smartMatrix.
	smartMatrix<-list(set = set, get = get,
           setInverse= setInverse,
           getInverse= getInverse)
	
	##Retrun the smartMatrix
	invisible(smartMatrix)
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
	
	##Read the cached inverse.
	inverse<-smartMatrix$getInverse()
	if(!is.null(inverse)){
		##Inverse is has been cached, return it and exit. 
		message("using smartMatrix's cached inverse")
		return(inverse)
	}
	
	##Inverse hasnt ben cached fetch the matrix for inversion 
	##from the 'smartMatrix'
	matrix2Invert<-smartMatrix$get()

	##Invert the Matrix
	inverse<-solve(matrix2Invert)

	##Cache the inverted matrix
	smartMatrix$setInverse(inverse)
	
	## Return the inverse of 'smartMatrix'
	inverse	     
}
