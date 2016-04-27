## Author:  Ray Bem 04.27.2016
## The following functions will work together to store in "cache" the solution
##   to the inverse of a supplied matrix.  The "cache" area is checked before 
##   any calculation is done.  To find the inverse matrix, the R solve function 
##   is used.

## makeCacheMatrix is a function of 4 functions, set, get, setinverse, getinverse.
##   The output is a list, which will be picked up by the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
        # check input, stop and message if not a matrix
        if (!is.matrix(x)) stop ("Input must be a matrix!")
        inverted_matrix <- NULL
        set <- function(y) {
                x <<- y
                inverted_matrix <<- NULL
        }
        get <- function() x
        # function to assign the solved original matrix to inverted_matrix
        # in "cache"
        setinverse <- function(from_cacheinverse_calc) {
                inverted_matrix <<- from_cacheinverse_calc
                message("Variable inverted_matrix set in cache from cacheinverse function")
        }
        getinverse <- function() inverted_matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve is a function that will check for a previously solved matrix (inverse),
##   and, if found, will use it.  If not found, cacheSolve will calculate the inverse
##   as well as update the "cache" version via the setinverse call.

cacheSolve <- function(x, ...) {
        # grab inverted matrix if available
        inverted_matrix <- x$getinverse()
        # if it exists, use it
        if(!is.null(inverted_matrix)) {
                message("Getting inverted matrix from cached data")
                return(inverted_matrix)
        } else {
        # if not qvailable, obtain original data
        original_matrix <- x$get()
        message("No cached inverse found, solving now and updating cache version")
        inverted_matrix <- solve(original_matrix)
        x$setinverse(inverted_matrix)
        return(inverted_matrix)
        }
}
