# Assignment for R programming Week 3 Lexical Scoping; for peer review assignment

makeCacheMatrix <- function(x = matrix()) #this function makes the special matrix in which it caches the data's inverse as shown in getinverse and setinverse
{
  minv <- NULL ##this is where the cached information will be stored; as assigned, it is named variable minv
  set <- function(y){ ##this serves to make the function in order to set the values in this given matrix
    x <<- y  ## the <<- indicates that it is assigning a value in a special environment, which is the parent one
    minv <<- NULL
}
  
# set and get will retrieve the values of the assigned matrix; getinverse and setinverse will calculate the inverse of these retrieved values
get <- function()x
setInverse <- function(inverse)minv <<- inverse
getInverse <- function()minv 
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# the function below then returns the inverse of the matrix above as calculated

cacheSolve <- function(x, ...) 
{
  minv <- x$getInverse()
  if(!is.null(minv)){
    message("getting cached data")
    return(minv)
  }
  mat <- x$get()
  minv <- solve(mat,...)
  x$setInverse(minv)
  minv
}
