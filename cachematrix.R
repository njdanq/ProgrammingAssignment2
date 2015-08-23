## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes matrix variables and sets them and functions in memory, 
## Then returns a list of functions within makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
  matrix_runs<- NULL                                              ## Set cache matrix runs to null
  set <- function(y) {                                            ## set function to store the matrix passed in the call as x and NULL as m, both in cache.
    cache_x <<- y                                   
    cache_m <<- NULL                                
  }
  get <- function() cache_x                                       ## function to get/return the matrix passed in the command line call to '$set
  set_cache_m <- function(matrix_runs) cache_m <<- matrix_runs    ## function to set the value of cache_m in cache to the value of local_m passed in the call to '$set_cache_m.        
  get_cache_m <- function() cache_m                               ## function to retrieve value of cache_m from cache and return cache_m to the caller so we can check it for NULL
  list(set = set, get = get,
       set_cache_m = set_cache_m,
       get_cache_m = get_cache_m)
}


## cacheSolve function receives a matrix from makeCacheMatrix() and inverts it and returns the inverted matrix

cacheSolve <- function(x, ...) {
      matrix_runs<- x$get_cache_m()                     ## Get value for m in the cache environment
  if(!is.null(matrix_runs)) {                           ## Check to see if m is Null.  
    message("getting cached data")                      ## If m is not Null, return the value of m with a message.
    return(matrix_runs)
  }                                                     
  starting_matrix <- x$get()                            ## Call function x$get in makeCacheMatrix to obtain the matrix with which to start assign it to starting_matrix.                         
  ending_matrix <- solve(starting_matrix)               ## Use solve() to invert the starting_matrix.  Assign the result to ending_matrix.
  x$set_cache_m(ending_matrix)                          ## Call nested function x$set_cache_m() in makeCacheMatrix to set m in the cache environment to the erted result of ending_matrix
  ending_matrix                                         ## Return a matrix that is the inverse of 'x'
}
