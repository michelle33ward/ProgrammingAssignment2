#The first function, makeCacheMatrix creates a special "vector", which is really a list of functions to
#   set the values of a matrix
#   get the values of matrix
#   set the value of the inverse of a function
#   get the value of the inverse of a function

makeCacheMatrix <- function(x = matrix()) { #pass makeCacheMatrix x which is a matrix
  inv <- NULL                               # Setting inv=NULL shows inv is empty 
  set <- function(y) {                      # function that assigns
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  #pass back a list that contains the four functions defined above.  
  list(set = set,get = get,setInverse = setInverse,getInverse = getInverse) 
}
#CacheSolve will check to see if the inverse of a matrix has been done already.  If it has then it will 
#not recompute the inverse but instead get it from cache.  If the inverse has not been computed, the 
#inverse will be computed and stored in inv.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Not computing inverse, getting cached data instead")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setInverse(inv)
  inv
}