## The pair of functions, makeCacheMatrix and cachesolve, can be used to cache
## inverse of a matrix, so it does not have to be computed repeatedly

## makeCacheMatrix creates a special "matrix" that that can be used to cache the
## inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setinvmatrix <- function(invmatrix) im <<- invmatrix
    getinvmatrix <- function() im
    list(set = set, get = get, 
         setinvmatrix = setinvmatrix,
         getinvmatrix = getinvmatrix)
}


## cacheSolve will compute the inverse of a "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed,
## the inverse is retrieved from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    im <- x$getinvmatrix()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    else {
        data <- x$get()
        ## rows and columns must be the same to create an inverse
        im <- solve(data)
        x$setinvmatrix(im)
        im
    }
}