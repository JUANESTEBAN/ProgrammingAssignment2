## 
## 

## The function makeCacheMatrix creates a special matrix, which is really a list 
## containing a function to  set the value of a matrix, get the value of a matrix, 
## set the value of the inverse, and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getmean <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## The function cacheSolve calculates the inverse of the special matrix created with the 
## makeCacheMatrix function. It checks if the inverse is calculated already. If the inverse was
## calculated, it gets the inverse from the cache and skips the computation. Otherwise, the 
## inverse of the matrix is calculated from the data and sets the value of the inverse in the 
## cache via the setinv function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m 
}