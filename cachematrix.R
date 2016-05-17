## Creates a matrix that caches it's inverse when it is calculated using the
## cacheSolve() function

## Creates a matrix that can cache it's own inverse. 
## Returns a list of functions that operate on the matrix.
makeCacheMatrix <- function(x = matrix()) {
     m_inverse <- NULL
     set <- function(y) {
          x <<- y
          m_inverse <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) m_inverse <<- solve
     getinverse <- function() m_inverse
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Computes the inverse of a matrix using cached matrix if available, using a
## makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}
