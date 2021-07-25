## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
