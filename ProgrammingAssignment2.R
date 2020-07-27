## R PROGRAMMING
#WEEK-3 #LEXICAL SCOOPING ASSIGNMENT


makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y = matrix()) {
    
    x <<- y*
      inv <<- NULL
      
  }
  
  get <- function() {x}
  setInverse <- function() {inv <<- inverse}
  getInverse <- function() {inv}
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
    
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
}