## makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the invert matrix
## get the value of the invert matrix
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        invertMatrix <- NULL
        
        set <- function(y) {
                x <<- y
                invertMatrix <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) invertMatrix <<- solve
        getInverse <- function() invertMatrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function calculates the inverse matrix of the special "matrix" created with the above function.
## It first checks to see if the inverse matrix has already existed.
## If so, it gets the matrix from the cache and skips the computation.
## Otherwise, it calculates the inverse matrix of the matrix and 
## sets the inverse matrix in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invertMatrix <- x$getInverse()
        if(!is.null(invertMatrix)) {
                message("getting cached data")
                return(invertMatrix)
        }
        data <- x$get()
        invertMatrix <- solve(data, ...)
        x$setInverse(invertMatrix)
        invertMatrix
}
