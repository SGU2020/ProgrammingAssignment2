## This two functions work together to cache the inverse of a matrix. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse.matrix <- NULL
  set <- function(y) {
    x <<- y
    inverse.matrix <<- NULL
  }
  get <- function() x
  setmean <- function(my.inverse) inverse.matrix <<- my.inverse
  getmean <- function() inverse.matrix
  list( set = set, get = get,
        setmean = setmean,
        getmean = getmean)
}


## This function computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse.matrix <- x$getmean()
  if(!is.null(inverse.matrix)) {
    message("getting cached data")
    return(inverse.matrix)
  }
  data <- x$get()
  inverse.matrix <- solve(data, ...)
  x$setmean(inverse.matrix)
  inverse.matrix
}
