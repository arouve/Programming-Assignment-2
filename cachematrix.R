## This function creates a matrix to cach it's reverse.

makeCacheMatrix <- function(x = matrix()) {
      
      temp1 <- NULL
      set <- function(y) {
            x <<- y
            temp1 <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) temp1 <<- inverse
      getInverse <- function() temp1
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
      
}


## This function calculates the inverse of the matrix created
## above. If the inverse is already calculated (and the matrix has not changed)
## then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
      
      ## Return a matrix that is the inverse of 'x'
      temp1 <- x$getInverse()
      if (!is.null(temp1)) {
            message("getting cached data")
            return(temp1)
      }
      
      mat <- x$get()
      temp1 <- solve(mat, ...)
      x$setInverse(temp1)
      temp1
}
