## Matrix inversion being a costly computation can be cached instead of computing repeatedly

##The makeCacheMatrix function performs the following operations:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
      matrix_inverse <- NULL
      set <- function(y) {
        x <<- y
        matrix_inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) matrix_inverse <<- inverse
      getinverse <- function() matrix_inverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## The cacheSolve function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    matrix_inverse <- x$getinverse()
    if(!is.null(matrix_inverse)) {
      message("getting cached data")
      return(matrix_inverse)
    }
    data <- x$get()
    matrix_inverse <- solve(data)
    x$setinverse(matrix_inverse)
    matrix_inverse
}
