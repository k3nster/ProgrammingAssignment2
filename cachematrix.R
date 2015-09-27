
## Cache matrix inverse 

makeCacheMatrix <- function(x = matrix()) {
  ## initialize
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  
  setsolve <- function(solve) i <<- solve
  
  getsolve <- function() ia
  
  list (set = set, get = get, 
        setsolve = setsolve, 
        getsolve=getsolve)
}

cacheSolve <- function(x, ...) { 
  
  # return a matrix that is the inverse of x
  i <- x$getsolve()
  if (! is.null(i)) {
    # return cache value if found
#     message("getting cache data")
    return(i)
  }
  
  # or find the invese and cache it for next time
  data <- x$get()
  i <- solve(data, ...)
  
  x$setsolve(i)
}
