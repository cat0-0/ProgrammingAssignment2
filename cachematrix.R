## Below are two functions that cache the inverse of a matrix.
## Matrix inversion is said to be a costly computation and it may be useful 
## to cache the inverse of a matrix instead of computing it repeatedly. 

## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse. It creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        invmtx <- NULL
        setmatrix <- function(y) {
                x <<- y
                invmtx <<- NULL
        }
        getmatrix <- function() x
        setinversematrix <- function(inverse) inv <<- inverse
        getinversematrix <- function() invmtx
        list(setmatrix=setmatrix, getmatrix=getmatrix, 
             setinversematrix=setinversematrix, 
             getinversematrix=getinversematrix)        
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve function 
## below retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        invmtx <- x$getinversematrix()
        if(!is.null(invmtx)) {
                message("getting cached data")
                return(invmtx)
        }
        data <- x$getmatrix()
        invmtx <- solve(data, ...)
        x$setinversematrix(invmtx)
        invmtx
}
## Sample Run: Return a matrix that is the inverse of 'x'
## x = matrix(1:4, nrow = 2, ncol = 2)
## m = makeCacheMatrix(x)
## x
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## cacheSolve(m)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
