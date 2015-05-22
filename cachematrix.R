##A special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix ()) {  # Defining to make matrix
  m <- NULL                                   # Create a NULL Matrix
  set <- function(y) {
    x <<- y                                   
    m <<- NULL
  }
  get <- function() x                         # Call the function
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,                  #Store the out put in list
       setinv = setinv,
       getinv = getinv)
}

## computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
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

