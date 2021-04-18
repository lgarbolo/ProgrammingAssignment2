## Assignment for R programming Week 3

## This function caches the inverse of the special matrix created 

makeCacheMatrix <- function(x = matrix()) {
  k <- NULL
  set <- function(y){
    x <<- y
    k <<- NULL
}
## setinverse and getinverse are used in order to calculate the inverse of the matrix
## meanwhile set and get are used to simply get the values of the assigned matrix
get <- function()x
setInverse <- function(inverse) k <<- inverse
getInverse <- function() k 
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## the function below then returns the calculated inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  k <- x$getInverse()
  if(!is.null(k)){
    message("getting cached data")
    return(k)
  }
  mat <- x$get()
  k <- solve(mat,...)
  x$setInverse(k)
  k
}
