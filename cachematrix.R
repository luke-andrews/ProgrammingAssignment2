# Author: Luke Andrews
# Date: 22-Feb-2015
# Version control managed by GitHub

# The objective of these two functions (makeCacheMatrix & cacheSolve) enables
# caching of a matrix inverse to save computing time. It takes advantage of 
# R's lexical scoping feature to cache.

# There are two functions.
# First function is makeCacheMatrix
# The objective of this matrix is to cache a matrix inverse.

# It works as follows:
# 1. Sets the matrix
# 2. Retrieve the matrix
# 3. Set inverse of matrix using solve function
# 4. Retrieve inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  matrix_inv <- NULL
  set <- function(y) { 
    x <<- y # set matrix using scoping
    matrix_inv <<- NULL
  }
  get <- function() x # retrieve matrix
  set_matrix_inv <- function(solve) matrix_inv <<- solve(x) # set inverse
  get_matrix_inv <- function() matrix_inv # retrieve inverse
  list ( set = set , get = get
         ,set_matrix_inv = set_matrix_inv,get_matrix_inv = get_matrix_inv)
}


# The second function is cacheSolve
# It works as follows:
# 1. Checks to see if matrix inverse is already stored in cache
# 2. If stored in cache, it retrieves the inverse and doesn't need to re-compute
# 3. IF not found in cache, it will calculate the inverse of the matrix x
# Matrix inverse is calculated using the solve function where required

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'

# 1. Check to see if already cached and retrieve if stored
matrix_inv <- x$get_matrix_inv()
if(!is.null(matrix_inv))
{
message("Getting cached data.")
return(matrix_inv)
}

# If not stored in cache, calculate inverse and then stores it in the cache
# for later use.
data <- x$get()
matrix_inv <- solve(data)
x$set_matrix_inv(matrix_inv)
matrix_inv
}
