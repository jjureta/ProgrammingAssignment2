## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

## Make a helper object (a list) for cached inverse
## x - a matrix for which the inverse can be cached

makeCacheMatrix <- function(x = matrix()) {
  ## set cache to null inorder to be sure that the cache can be correctly validated
  ## and also to set inverse_cache into current context
  inverse_cache <- NULL
  
  ## cache set function
  ## y - a matrix for which the inverse is calculated
  set <- function(y) {
    ## the superassignment (see: http://bio-statistician.com/use-the-superassignment-operator.html)
    ## a function is only able to change a variable from its own context,
    ## the superassignment change/creates a variable in higher context
    x <<- y
    inverse_cache <<- NULL
  }
  
  ## cache get function. returns the matrix for which the inverse is calculated
  get <- function() x
  
  ## put the inverse into the cache
  setinverse <- function(inverse) inverse_cache <<- inverse
  
  ## get the inverse from the cache
  getinverse <- function() inverse_cache
  
  ## return helper list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculate the inverse of given matrix. If the inverse of given matrix
## was already calculated then return it from the cache; if not calculate it and
## put it into the cache
## x - the list created by 'makeCacheMatrix'

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## verify if the inverse was already calculated
  m <- x$getinverse()
  if(!is.null(m)) {
    ## remove debug purpose message
    message("getting cached data")
    
    ## the inverse was already calculated, get it from the cache
    return(m)
  }
  
  ## the inverse ws not calculated
  ## get matrix for which the inverse must be calculated
  data <- x$get()
  ## calculate the inverse
  m <- solve(data, ...)
  ## put the inverse into the cache
  x$setinverse(m)
  ## return calculated inverse (next time the inverse will be taken from the cache)
  m
  
}
