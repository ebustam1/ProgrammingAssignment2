# Matrix inversion is a costly computation and there may be some 
# benefit to caching the onverse of a matrix rather than computing 
# computing it repeteadly. The following functions are used to
# cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of matrix
# 2. get the value of matrix
# 3. set the value of inverse of matrix
# 4. get the value of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {

inv <- NULL
set <- function (y) {
	x <<- y
	inv <<- NULL
}
get <- function() x
setinverse <- function(inverse) inv <<- solve(x)
getinverse <- function() inv
list(set = set,
 get = get,
 setinverse = setinverse,
getinverse = getinverse)
}

# The cacheSolve function returns the inverse of the matrix. First,
# it checks if the inverse has already been computed.  If the 
# inverse has been computed, it gets the result and skips the computation.  If not, it computes the inverse and sets the value in the cache.  

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
	}
        ## Return a matrix that is the inverse of 'x'
## Sample run:
## > a <- makeCacheMatrix(matrix(1:4,2))
## > a$get()
##     [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > a$getinverse()
## NULL
## > a$set(matrix(5:8, 2))
## > a$get()
##    [,1] [,2]
## [1,]    5    7
## [2,]    6    8
## > cacheSolve(a)
##     [,1] [,2]
## [1,]   -4  3.5
## [2,]    3 -2.5
## > cacheSolve(a)
## getting cached data
##    [,1] [,2]
## [1,]   -4  3.5
## [2,]    3 -2.5
## > a$getinverse()
##     [,1] [,2]
## [1,]   -4  3.5
## [2,]    3 -2.5
## > b = a$getinverse()
## > a$get() %*% b
##     [,1]         [,2]
## [1,]    1 3.552714e-15
## [2,]    0 1.000000e+00