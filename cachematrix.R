## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Assignment: Caching the Inverse of a Matrix
## The assignment is to write a pair of function that cache
## makeCacheMatrix:creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

## Initialize the inverse property
  m<-NULL
  
  ## Set the matrix
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  ## Get the matrix
  get <- function() {
    ## Return the matrix
    x
  }
  ## Set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  ## Get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    m
  }
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        ## Return the inverse if its already set
        if( !is.null(m) ) {
          message("getting cached data")
          return(m)
        }
        ## Get the matrix from our object
        data <- x$get()
        ## Calculate the inverse using matrix multiplication
        m <- solve(data) %*% data
        ## Set the inverse to the object
        x$setInverse(m)
        ## Return the matrix
        m      
}
