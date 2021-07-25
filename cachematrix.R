################################################################################
## Functions to solve a matrix using a cache system to impove performances    ##
################################################################################

## Function makeCachematrix :
##
## makeCacheMatrix initialize the cache used to solve a matrix. The cache is 
## able to manage two matrixes : the original matrix and its inverse.
## Four functions are returned by makeCacheMatrix :
##  - get and set : to get and set the original matrix in the cache
##  - get_inverted_matrix and set_inverted_matrix to get and set the inverted 
##    matrix in the cache
makeCacheMatrix <- function(x = matrix()) {
  x_1 <- NULL
  set <- function(mt) {
    x <<- mt
    x_1 <<- NULL
  }
  get <- function() x
  set_inverted_matrix <- function(inverted_matrix) x_1 <<- inverted_matrix
  get_inverted_matrix <- function() x_1
  list(set = set, get = get,
       set_inverted_matrix = set_inverted_matrix,
       get_inverted_matrix = get_inverted_matrix)
}


## Function cacheSolve :
##
## cacheSolve takes as inpout the resulting functions from a call to 
## makeCacheMatrix. And then it computes and returns the inverse of the original 
## matrix. The inverse matrix :
##  - is computed if the result is not already in the cache
##  - is returned directly from the cache if it was previously computed
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverted_matrix <- x$get_inverted_matrix()
  if(!is.null(inverted_matrix)) {
    message("getting inverted matrix from cached data")
    return(inverted_matrix)
  }
  inverted_matrix <- solve(x$get(), ...)
  x$set_inverted_matrix(inverted_matrix)
  inverted_matrix
}
