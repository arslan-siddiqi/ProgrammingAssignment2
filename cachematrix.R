## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {        
   
          inv <- NULL           #initializing inverse as null
  set <- function(y = matrix()) {
    
    x <<- y*
      inv <<- NULL
      
  }
  
  get <- function() {x}                      #This is the function to get matrix
  setInverse <- function() {inv <<- inverse}
  getInverse <- function() {inv}                #function is to obtain the inverse of the matrix
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
                 
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)          #return inverse value
    
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
}
