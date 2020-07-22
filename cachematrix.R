## cacheSolve uses functions defined in makeCacheMatrix to store or access
## inverse of matrix to avoid having to recalculate it repeatedly.

## This function stores matrix and its inverse,
## returns a list of function used to access them.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function (y = matrix()) {
        x <<- y
        inverse <<- NULL
        }
    get <- function() x
    put_inverse <- function(y = matrix()) inverse <<- y
    get_inverse <- function() inverse
    list(set=set, get=get, put_inverse=put_inverse,
         get_inverse=get_inverse)
}

## This function retrieves inverse of matrix in cache, 
## else computes and caches it if not cached previously.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inverse()
    if (!is.null(inv))
        message("getting cached inverse matrix")
    else {
        inv <- solve(x$get(), ...)
        x$put_inverse(inv)
    }
    inv
}
