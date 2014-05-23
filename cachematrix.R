## This excercise contains two functions:
## - makeCacheMatrix(x) caches the inverse of a matrix object,
## - cacheSolve, which does the inverting and commits it to the cache.

## This function takes a square matrix as a parameter, and returns a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  # takes a matix, and sets it as the value to calculate the inverse of
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # returns the value to calculate the inverse of
  get <- function() x
  
  # caches the inverst of the matrix
  setinverse <- function(inverse) i <<- inverse
  
  # gets the cached value of the matrix, or NULL if it hasn't been cached yet
  getinverse <- function() i
  
  # return a list with all the function in the object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes a makeCacheMatrix object.
## The first time it's run, it calculates the inverse of that matrix with `solve`,
## caches it, and returns it. If run again it returns that cached value.
cacheSolve <- function(x, ...) {
  
  # Get the cached inverse from the makeCacheMatrix object
  inverse <- x$getinverse()
  
  # If it's not null, return it, along with a message that you've gotten it from the cache
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # If we've gotten here, it wasn't cached, so get the matrix...
  data <- x$get()
  
  # calculate its inverse...
  inverse <- solve(data)
  
  # cache it...
  x$setinverse(inverse)
  
  # and return it.
  inverse
}
