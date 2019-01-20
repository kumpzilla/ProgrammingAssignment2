# makeCacheMatrix and cacheSolve operate together to create, from an input matrix, a "matrix" 
# object containing functions that can cache the inverse of the input matrix. To save unnecessary 
# computation time, the inverse is only calculated if it has not yet been done for the current
# input data or if the input data have been changed since the last inverse was calculated  (i.e., 
# the inverse currently has a NULL value in the cache).

##makeCacheMatrix creates a "matrix" object consisting of 
# 1) the input data (an invertible square matrix): mat2inv
# 2) the inversion of that matrix: invMat
# 3) a set of 4 functions to cache and retrieve both the input data and the inverted matrix. 
makeCacheMatrix <- function(mat2inv = matrix()) {
  invMat <- NULL #initialise
  set_Cache.mat2inv <- function(y) {
    mat2inv <<- y
    invMat <<- NULL
  } # a function to cache a new matrix to be inverted
  set_Cache.invMat <- function(z) {
    invMat <<- z
  } # a function to cache the inverted matrix
  get_Cached.mat2inv <- function() mat2inv # a function to get the matrix to be inverted
  get_Cached.invMat <- function() invMat # a function to get the inverted matrix
  list(set_Cache.mat2inv=set_Cache.mat2inv, 
       set_Cache.invMat=set_Cache.invMat,
       get_Cached.mat2inv=get_Cached.mat2inv, 
       get_Cached.invMat=get_Cached.invMat) #output as a list
}
##cacheSolve computes the inverse of the matrix object returned by makeCacheMatrix, if it has
# not already been done for the current input data. Otherwise, cacheSolve retrieves the inverse 
# from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMat <- x$get_Cached.invMat()
  if(!is.null(invMat)) {
    message("getting cached data")
    return(invMat)
  } # if the cache is not empty, ie inverse already computed for this matrix, return that inverse
  data2inv <- x$get_Cached.mat2inv()
  invMat2cache <<- solve(data2inv) # else invert the data in mat2inv
  x$set_Cache.invMat(invMat2cache) # cache the new inversion
  invMat2cache # and print
}
