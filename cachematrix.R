## IM = Inverse Matrix
## function that has internal methods for access to matrix & cache methods.
## setIMCache must be call to set cache of Inverse Matrix
## getIMCache must be call for get cache result.
## the last line with list() Expose all functions that can be access.
makeCacheMatrix <- function (x = matrix()) {
  cacheIM <- NULL
  setMatrix <- function (y){
    x <<- y
    cacheIM <<- NULL
  }
  getMatrix <- function() x
  setIMCache <- function ( calculatedIM ) cacheIM <<- calculatedIM
  getIMcache <- function() cacheIM
  list( setMatrix = setMatrix, getMatrix = getMatrix, setIMCache = setIMCache, getIMcache = getIMcache)
}

## Get inverse matrix from cache, if was cached return the cache result,
## else calculate the matrix inverse, set to cache and return the result.
cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getIMcache()
  if(!is.null(inverseMatrix)) {
    message("Getting inverse matrix from cache")
    return(inverseMatrix)
  }
  inverseMatrix <- solve(x$getMatrix(), ...)
  x$setIMCache(inverseMatrix)
  inverseMatrix
}