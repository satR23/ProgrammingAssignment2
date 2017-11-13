## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly.

## Following are functions that can be used to perform the inverse computation. 


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
              setInverse = setInverse,
              getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the content of the matrix stays the same, then
## inverse gets retrieved from the cache, if not a new computatuion is performed
## to get the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        inv <- x$getInverse()
        
        ## a value is returned if in the cache
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

> m <- makeCacheMatrix(matrix(1:4,2,2))
> m$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> m$getInverse()
NULL
> cacheSolve(m)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(m)
getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> m <- makeCacheMatrix(matrix(c(2,3,1,5),2,2))
> m$get()
     [,1] [,2]
[1,]    2    1
[2,]    3    5
> m$getInverse()
NULL
> cacheSolve(m)
           [,1]       [,2]
[1,]  0.7142857 -0.1428571
[2,] -0.4285714  0.2857143
> cacheSolve(m)
getting cached data
           [,1]       [,2]
[1,]  0.7142857 -0.1428571
[2,] -0.4285714  0.2857143
> m$getInverse()
           [,1]       [,2]
[1,]  0.7142857 -0.1428571
[2,] -0.4285714  0.2857143
