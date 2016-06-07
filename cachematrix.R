## 6/7/2016 -- DataScience R Programming Assignment for Week3:  Caching the Inverse of a Matrix
##  https://www.coursera.org/learn/r-programming/peer/tNy8H/programming-assignment-2-lexical-scoping
##  
##  Matrix inversion is usually a costly computation and there may be some benefit to 
##     caching the inverse of a matrix rather than compute it repeatedly 
##
##  The assignment is to write a pair of functions that cache the inverse of a matrix.
##


## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##   For this assignment, assume that the matrix supplied is always invertible.
##   Sample call:
##     > m1 <- matrix(c(4,3,3,2),2,2)
##     > makeCacheMatrix(m1)


makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) m <<- inverse
            getinverse <- function() m
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)

}


## 2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##    If the inverse has already been calculated (and the matrix has not changed), 
##    then the cachesolve should retrieve the inverse from the cache.
##   Sample call:
##     > m2 <- makeCacheMatrix(m1)
##     > cacheSolve(m2)


cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'

            m <- x$getinverse()
            if(!is.null(m)) {
                    message("getting cached data - inverse")
                    return(m)
            }
            data <- x$get()
			
##  Computing the inverse of a square matrix can be done with the solve function in R. 
##  For example, if X is a square invertible matrix, then solve(X) returns its inverse.
			
            m <- solve(data, ...)
            x$setinverse(m)
            m

}
