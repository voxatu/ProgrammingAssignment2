## The first function, makeCacheMatrix creates a special "matrix", which is
## really a list containing a functions to set the value of the matrix,
## get the value of the matrix, set the value of the inverse matrix,
## get the value of the inverse matrix.
##
## The second function calculates the inverse of the special "matrix" created
## with makeCacheMatrix function. However, it first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache and
## skips the computation. Otherwise, it calculates the inverse and
## sets the inverse matrix in the cache via the setinverse function.
##
##
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ix <- NULL
        set <- function(y) {
                x <<- y
                ix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) ix <<- inverse
        getinverse <- function() ix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then cacheSolve will retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        ix <- x$getinverse()
        if(!is.null(ix)) {
                message("getting cached data")
                return(ix)
        }
        data <- x$get()
        ix <- solve(data, ...)
        x$setinverse(ix)
        ix
}

## Functions usage example:
## > x <- matrix(c(1, 0, 1, 1), 2, 2)
## > x
##     [,1] [,2]
##[1,]    1    1
##[2,]    0    1
## ## We must test first if the supplied matrix is invertible
## > det(x)
## [1] 1
## ## Determinant of the matrix is not equals to 0, so the matrix is invertible
## > cm <- makeCacheMatrix(x)
## > cacheSolve(cm)
##     [,1] [,2]
##[1,]    1   -1
##[2,]    0    1
## > cacheSolve(cm)
##getting cached data
##     [,1] [,2]
##[1,]    1   -1
##[2,]    0    1
## ## The test has been passed.
## ## Let's check what happens when we multiply the matrix by its inverse
## > cm$get() %*% cm$getinverse()
##     [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## ## When you multiply a matrix by its inverse you get the identity matrix
