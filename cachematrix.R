## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


#This function is used to cache the results of inverse operation on a matrix
makeCacheMatrix <- function(x = matrix()) {
  
  #This variable is used to store/cache the inverse.
  inverse <- NULL
  
  #This is the set method , which is used to set the data.
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  #This method is used to return raw data
  get <- function() x
  #This method stores the inverse value
  setinverse <- function(inv) inverse <<- inv
  #This method returns the inverse value
  getinverse  <- function() inverse
  #This call return a list containing the various functions.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
#CacheSolve obtains the inverse for a matrix, it first tries to read the
#cached solve matrix, if it fails it would solve the cache the data.
#and later return the same.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  #If cache exist return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #No cache exist, lets get raw data and solve it .
  data <- x$get()
  m <- solve(data,...)
  #cache the inverse before returning.  
  x$setinverse(m)
  m
}
