## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## It stores a matrix and a cached value of the inverse of the matrix 
# It has following functions: set, get, setinversem, getinversem

makeCacheMatrix <- function(x = matrix()) {
## computing inverse matrix for a given matrix.
m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversem <- function(solve) m <<- solve
  getinversem <- function() m
  list(set = set, get = get,
       setinversem = setinversem,
       getinversem = getinversem)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse
## has already been calculated and the matrix has not changed, then cacheSolve function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## if an inverse matrix has already been calculated for the given matrix, it retrives the answer from cache.
  m <- x$getinversem()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## if a new matrix is given, it calculates the inverse.
  data <- x$get()
  m <- solve(data, ...)
  x$setinversem(m)
  m
}
