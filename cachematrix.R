## The 1st function creates a special "matrix" object that can cache its inverse.
## The 2nd computes the inverse of the special "matrix" returned by the 1st function
## Together, they are a pair of functions that cache the inverse of a matrix.

## makeVector creates a special "matrix" object that can cache its inverse.
## This special "matrix" is an input argument for cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
    InvertedMatrix <- NULL
    set <- function(y) {
        x <<- y
        InvertedMatrix <<- NULL
    }
    get <- function() x
    SetInverse <- function(Inverse) InvertedMatrix <<- Inverse
    GetInverse <- function() InvertedMatrix
    list(set = set, get = get, SetInverse = SetInverse, GetInverse = GetInverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been found, the function will return the cached
## matrix inverse.

cacheSolve <- function(x, ...) {
    InvertedMatrix <- x$GetInverse()
    if(!is.null(InvertedMatrix)) {
        message("Getting cached matrix inverse.")
        return(InvertedMatrix)
    }
    InvertThisMatrix <- x$get()
    InvertedMatrix <- solve(InvertThisMatrix)
    x$SetInverse(InvertedMatrix)
    InvertedMatrix
}
