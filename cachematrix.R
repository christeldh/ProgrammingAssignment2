## Code for Assignment 2 of the R Programming Course of John Hopkins as offered through Coursera
## Author: C Hansen
## Date: May 2015

## This function is designed to cache potentially time-consuming computations
## The function consists of two separate functions designed to speed up the overall task

##The function below is used to create a special type of matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # Initial value/matrix set to NULL to prevent errors
  inv <- NULL # Setting the hypothetical inverse matrix to NULL to prevent errors
  
  set <- function(y)  {
    x <<- y 
    m <<-NULL 
  }
  
  get <- function () x  # Obtain the matrix
  makeinv <- function (solve) # Solve for the inverse of the matrix
  inv <<- solve     # Assign solved inverse to the variable 
  getinv <- function () inv # Function to catch inverse of matrix
  list(set = set, get = get, 
       makeinv = makeinv, getinv = getinv)
}

## The function below can compute the inverse of the matrix created in makeCacheMatrix
## If the inverse has been calculated in a previous occasion and the matrix has not changed
## The inverse is retrieved from the cache

cacheSolve <- function(x, ...) {        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv() # See if an inverse exists in the cache. Function is called from makeCahceMatrix
  if(!is.null(inv)){ # Has cacheSolve been run?
    message ("Inverse exists. Cache called.")
    if(x$set() == x$get()) { # Evaluate the state of the matrix: is it the same as before or it has changed
      return(inv) # If the matrix has not changed the inverse is returned
    }
    y <- x$get() # Run the get function to get the input matrix
    inv <- solve(y, ...) # Compute the value of the inverse of the input matrix
    x$makeinv(inv) # Run the set function on the inverse to cache the inverse
    inv # Return the inverse
    message("Inverse returned.")
  }
  