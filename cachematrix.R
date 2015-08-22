## This Script contains 2 functions : 
#  makeCacheMatrix() and cacheSolve() 
#  to cache and calculate the inverse of given matrix.

## The function makeCacheMatrix creates a list to
# 1) set the value of the matrix
# 2) get the value of the matrix
# 3) set the value of the inverse matrix
# 4) get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get, 
		setInverse = setInverse, 
		getInverse = getInverse)
}


## The function cacheSolve calculates the inverse of the matrix
#  created from the function makeCacheMatrix. 
#  
#  The function cacheSolve checks if the inverse has already been calculated.
#  If calculated, it gets the inverse from the cache skips the computation.
#  
#  Otherwise, 
#  it calculates the inverse of the matrix,
#  sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
