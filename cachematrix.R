## Followed the examples provided at: 
## https://github.com/DanieleP/PA2-clarifying_instructions

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      
      ## Initialize the inverse property
      m <- NULL
      
      ## Set the matrix and store in parent function
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      ##  Get the matrix "x" stored in the parent function 
      get <- function() {
            ## Return the matrix
            x
      }
      
      ## Set the inverse of the matrix and store in parent function
      set_inverse <- function(inverse) {
            m <<- inverse
      }
      
      ## Get the inverse of the matrix from parent function
      get_inverse <- function() {
            ## Return the inverse matrix
            m
      }
      ## store the four functions within the parent function
      list(set = set, get = get, set_inverse = set_inverse, 
           get_inverse = get_inverse)

}


## cacheSolve computes the inverse of output from makeCacheMatrix. 
## It should retrieve the inverse from the cache when appropriate.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$get_inverse()
      ## Check if matrix exists. If not, return cached value.
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      ## Get matrix stored with makeCacheMatrix function
      data <- x$get()
      ## calculate inverse using matrix multiplication
      m <- solve(data) %*% data
      ## store and display inverted matrix
      x$set_inverse(m)
      m
}
