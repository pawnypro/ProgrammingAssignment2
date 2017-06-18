## We take a matrix, and use lexical scoping to cache its inverse
## so that it can be directly fetched from the cache next time, and need not 
## be computed again, thereby saving computing resources. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
  i <- NULL
  set <- function(y = matrix())
  {
    x <<- y
    i <<- NULL
  }
  
  get <- function() 
    x
  
  setInverse <- function(inverse = matrix()) 
    i <<- inverse
  
  getInverse <- function() 
    i
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function checks if the inverse is already stored in cache and returns it as is
## if thats the case. Else, inverse is computed using the solve() function and returned.

cacheSolve <- function(x, ...) 
{
  i <- x$getInverse()
  
  if(!is.null(i)) 
  {
    message("getting cached data")
    return(i)
  }
  
  data <- matrix() 
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
  
}
