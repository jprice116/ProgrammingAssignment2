## These functions cache the inverse of a matrix so that it 
## does not have to be computed repeatedly.

## makeCacheMatrix creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	## input x will be a matrix
	 m <- NULL  
        ## m is reset to NULL every time makeCacheMatrix is called
        
        ## note these next three functions are defined 
        ## but not run when makeCacheMatrix is called.
        ## instead, they will be used by cacheSolve() to get values 
        ## for x or for m and for inverting the matrix.  
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## takes an input vector
        ## saves the input vector 
        ## resets the matrix to NULL
        
        get <- function() x
        ## this function returns the value of the original input
        
        setInverse <- function(Inverse) m <<- Inverse
        ## this is called by cacheSolve() during the first cacheSolve()
        ## access and it will store the value using superassignment
        
        getInverse <- function() m
        ## this will return the cached value to cacheSolve() on
        ## subsequent accesses
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        ## return a list of functions as an R object
}


## cacheSolve inverts the special "matrix" created with makeCacheMatrix.
## It first checks to see if the inverse matrix has already been created.
## If so, it gets the inverse matrix from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value
## in the cache via the setInverse function.


cacheSolve <- function(x, ...) {
        ## the input x is an object created by makeCacheMatrix
        
        m <- x$getInverse()
        ## accesses the object 'x' and gets the inverse matrix
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## if inverse was already cached (not NULL),
        ## send this message to the console... 
        ## and return the mean ... "return" ends 
        ## the function cacheSolve()
        
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        ## if m was NULL then we have to calculate the mean
        ## store the calculated mean value in x
        
        m
}
