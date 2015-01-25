## This par of functions to caching the inverse of a matrix. 

## This function creates a special 'matrix' object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
          x <<- y
          inv <<- NULL    
        
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special 'matrix' returned by function makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                 mesagge('getting cached data')
                 return(inv)
        }
        matr <- x$get()
        inv <- solve(matr, ...)
        x$setinv(inv)
        inv
}
