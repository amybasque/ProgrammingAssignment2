## Put comments here that give an overall description of what your
## functions do

## Function to set the value of a matrix, get the value of a matrix, 
## set the value of the inverse of the matrix, and get the value of 
## the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {

	    m <- NULL
	    ## Set value of the matrix. 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## Get the matrix. 
        get <- function() x
        ## Set the inverse of the matrix. 
        setinverse <- function(solve) m <<- solve
        ## Get value of inverse of matrix. 
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
        ## Check whether cached value exists. 
        if(!is.null(m)) {
                message("getting cached data")
                ## Return cached value. 
                return(m)
        }
        data <- x$get()
        ## Calculate inverse of matrix. 
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
