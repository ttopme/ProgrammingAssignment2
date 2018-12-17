## This program caches and the returns the inverse of an invertible square matrix


makeCacheMatrix <- function(x = matrix()) {
## This function caches the inverse of an invertible square matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

my_matrix <- makeCacheMatrix()
my_matrix$set(matrix(1:4, 2, 2))
my_matrix$get()
my_matrix$setinverse(solve(my_matrix$get()))
cacheSolve(my_matrix)
