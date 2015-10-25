## These functions together will create a square invertible matrix
## and cache its inverse

## creates and returns a list of functions used by cacheSove to get or set the
## inverse of the matrix in the cache

makeCacheMatrix <- function(x = matrix()) {
        # stores the cached value
        # initialize to NULL
        cache <- NULL
        
        # create the matrix in the working environment
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }
        
        # get the value of the matrix
        get <- function() x
        # invert the matrix and store in cache
        setInverseMatrix <- function(inverse) cache <<- inverse
        # get the inverted matrix from cache
        getInverseMatrix <- function() cache
        
        # return the created functions to the working environment
        list(set = set, get = get,
             setMatrix = setMatrix,
             getInverse = getInverse)
}


## Calculate the inverse of the matrix from makeCacheMatrix
## If the inverted matrix does not exist in the cache, it is created in the
## working environment and cached its inverted value

cacheSolve <- function(x, ...) {
        ## attempt to get the inverted matrix from cache
        cache <- x$getInverseMatrix()
        
        # return inverted matrix if it exists
        # if not then create the matrix 
        if (!is.null(cache)) {
                message("getting cached data")
                
                # display matrix in console
                return(cache)
        }
        
        # create matrix if it does not exist
        matrix <- x$get()
        
        # make sure matrix is square and invertible
        # if not, handle exception cleanly
        tryCatch( {
                # set and return inverse of matrix
                cache <- solve(matrix, ...)
        },
        error = function(e) {
                message("Error:")
                message(e)
                
                return(NA)
        },
        warning = function(e) {
                message("Warning:")
                message(e)
                
                return(NA)
        },
        finally = {
                # set inverted matrix 
                x$setInverseMatrix(cache)
        } )
        
        # display matrix
        return (cache)
}
