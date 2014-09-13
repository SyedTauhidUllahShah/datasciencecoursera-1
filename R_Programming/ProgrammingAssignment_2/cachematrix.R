## The following functions compute the inverse of a matrix and cache it in a
## special "matrix" object for easy retrieval

## makeCacheMatrix: This function creates a special "matrix" object that
## can cache its inverse.  It returns a list that contains a function that
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the matrix inverse
## 4. gets the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
     # Initialize NULL matrix 
     m <- NULL
     
     # Create set function to set the value of the matrix
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     
     # Create the get function for retrieval of the value of the matrix
     get <- function() x
     
     # Create setInverse function to set the value of the matrix inverse
     setInverse <- function(matInverse) m <<- matInverse
     
     # Create getInverse function to retrieve the value of the matrix inverse
     getInverse <- function() m
     
     # Return the matrix and cached inverse (if exists) as a list
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getInverse()
     
     # Check if matrix inverse exists. If so return the inverse and exit
     if(!is.null(m)) {
          message("getting cached inverse")
          return(m)
     }
     
     # If inverse does not exist, retrieve the matrix value, compute the
     # inverse, and set its value
     data <- x$get()
     m <- solve(data)
     x$setInverse(m)
     
     # Return the matrix inverse
     m
}
