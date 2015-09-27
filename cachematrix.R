## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function (x = matrix()) {
  m <- NULL
  set <- function (y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setIM <- function ( inverse ) m <<- inverse
  getIM <- function() m
  list( set = set, get = get, setIM = setIM, getIM = getIM)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getIM()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(m,  ...)
  x$setIM(m)
  m
}

m <- makeCacheMatrix(matrix(rnorm(9),3,3))
cacheSolve(m)
