## The two functions collectively compute the inverse of a matrix and 
## cache.

## The following function creates a matrix object, which is capable
## of chaching its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(x) {
            mat <<- x;
            inverse <<- NULL;
        }
        get <- function() return(mat);
        setinv <- function(inv) inverse <<- inv;
        getinv <- function() return(inverse);
        return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## The following function computes the inverse of a matrix. If the
## pre-calculated inverse is already available in the memory then it returns
## the inverse from memeory.

cacheSolve <- function(x, ...) {
    inverse <- x$getinv()
    if(!is.null(inverse)) {
        message("Getting data from cache...")
        return(inverse)
    }
    data <- x$get()
    invserse <- solve(data, ...)
    x$setinv(inverse)
    return(inverse)
}
