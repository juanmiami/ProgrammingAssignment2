## These R files Includes 2 functions -> 'makeCacheMatrix()'and 'cacheSolve()'
## to cache and calculate the inverse of a given matrix

##  Creates a special 'matrix' object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## return-> a list containing functions to
    ##              1. set the matrix
    ##              2. get the matrix
    ##              3. set the inverse
    ##              4. get the inverse

    
    inv = NULL
    set = function(y) {
        x <<- y
        inv <<- NULL
    }
    get = function() x
    setinv = function(inverse) inv <<- inverse 
    getinv = function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Computes the inverse of the “matrix” returned by makeCacheMatrix()
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

		inv = x$getinv()
		
		if (!is.null(inv)){
			# get it from cached value 
			message("getting cached data")
			return(inv)
		}
		
		# calculates the inverse 
		inv = solve( x$get(), ...)
		x$setinv(inv)
		return(inv)		
}