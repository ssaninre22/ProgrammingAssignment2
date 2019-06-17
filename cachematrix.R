
## The following function computes the cache structure for the inverse of a matrix X


## Write a short comment describing this function
makeCacheMatrix <- function(x=matrix()){
  matx <- NULL
  # Set the matrix
  set <- function(y){
    x <<- y
    matx <<- NULL
  }
  # Get the initial matrix
  get <- function() x
  # Create the inverse matrix
  setinverse <- function(inverse) matx <<- inverse
  # Get the inverted matrix
  getinverse <- function() matx
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function will compute the inverse of the special matrix provided by the last
## function, or will use a before computed inverse matrix of x if it was already 
## computed.

cacheSolve <- function(x, ...){
  ## Return a matrix that is the inverse of 'x'
  matx =x$getinverse()
  # Check if inverse already computed
  if(!is.null(matx)){
    message("getting cached data")
    return(matx)
  }
  # If inverse not already computed then compute it
  data =x$get()
  matx =solve(data, ...)
  x$setinverse(matx)
  matx
}

## EXAMPLE
x <- matrix(rnorm(16),nrow=4,ncol=4)
xx <- makeCacheMatrix(x)
xsolve <- cacheSolve(xx)

