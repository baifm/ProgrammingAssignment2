## A pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv.matrix <- NULL
    set <- function(y) {
        x <<- y
        inv.matrix <<- NULL
    }
    get <- function() matrix(x, nrow=2, ncol=2)
    set.inv.matrix <- function(inv.matrix.val) inv.matrix <<- inv.matrix.val
    get.inv.matrix<- function() inv.matrix
    list(set = set, get = get,
         set.inv.matrix = set.inv.matrix,
         get.inv.matrix = get.inv.matrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$get.inv.matrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$set.inv.matrix(m)
    m
}
