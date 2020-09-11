## There are 2 functions that allows the user to calculate de inverse of a square matrix
## and store it so it can be retrieved when necessary without the need of solving it every time.

## makeCacheMatrix: Creates an object that stores the matrix passed
## and allows it's inverse to be cached and retrieved.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinvers <- function(invers) i <<- invers
  getinvers <- function() i
  invisible(list(set = set, get = get,
       setinvers = setinvers,
       getinvers = getinvers))
}


## cacheSolve: It accepts a list returned by the "makeCacheMatrix" function
## and returns the inverse of the matrix while also storing it inside the list argument.

cacheSolve <- function(x, ...) {
  i <- x$getinvers()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinvers(i)
  i
        
}
