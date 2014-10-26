## Put comments here that give an overall description of what your
## functions do

## This function calculates the matrix inverse, and stores it.

makeCacheMatrix <- function(x = matrix()) {
        
        ## initialize the inverse matrix
        m <- NULL
        
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        
        get <- function() x
        
        ## Store the inverse outside of this environment
        setinverse <- function(solve) m <<- solve
        
        ## Return the value stored in this vector
        getinverse <- function() m
        
        ## List of available functions in this vector
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function sees if the matrix inverse is cached
## And returns the cache if so, or calculate and stores it, if not

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        ## if the vector returned a value, it is cached
        if(!is.null(m)) {
                ## And Exit
                return(m)
        }
        
        data <- x$get()
        
        ## Solve the inverse
        m <- solve(data, ...)
        
        ## Store the inverse
        x$setinverse(m)
        
        m
}
