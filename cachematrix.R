## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # initialize the variable
  inv <- NULL
  
  # set a matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get a matrix
  get <- function() x
  
  # set the inverse of the matrix
  setinver <- function(inver) inv <<- inver
  
  # get the inverse of the matrix
  getinver <- function() inv
  
  # return all the variables to the function
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
   ## Return a matrix that is the inverse of 'x'
  inv <- x$getinver()
  
  ## choose the computable matrix
  if(!is.null(inv) ) {
    message("getting cached data")
    return(inv)
  }
  
  # get the matrix
  data <- x$get()
  
  # inverse computation
  inv <- solve(data, ...)
  x$setinver(inv)
  inv
}
