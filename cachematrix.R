## Cachematrix.R Week 3 Programming Assignment 2

## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function(y)  {
				x <<- y
				inv <<- NULL
		}
		get <- function() x
		setinv <- function(inv) inv <<- inverse
		getinv <- function() inv
		list(set = set, get = get, setinv = setinv, getinv = getinv)
		
}


## Computes the inverse of matrix returned by makeCacheMatrix. If already calculated it will return answer from cache rather than recalculate and saving processing time

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv))  {
        	message("getting cached data")
        	return(inv)
        }
        mat.data <- x$get
        inv <- solve(mat.data, ...)
        x$setinv(inv)
        return(inv)
}
