## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinver <- function(inver) inv <<- inver
  getinver <- function() inv
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinver()
  d = dim(inv)
  if(d[1] == d[2] ) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinver(inv)
  inv
}
