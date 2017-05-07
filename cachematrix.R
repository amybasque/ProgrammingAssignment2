## Put comments here that give an overall description of what your
## functions do

## Function to set the value of a matrix, get the value of a matrix, 
## set the value of the inverse of the matrix, and get the value of 
## the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {

	    m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Function to check whether cached value of matrix inverse exists
## and returns it if it does or calculates the inverse if not. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
