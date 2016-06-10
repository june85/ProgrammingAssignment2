# makeCacheMatrix creates a special "matrix", which is really a list
# containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  mtrx <- NULL
  set <- function(y) {
    x <<- y
    mtrx <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) mtrx <<- solve
  getSolve <- function() mtrx
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

## The following function calculates the mean of the special "vector" created
## with the above function. However, it first checks to see if the mean has
## already been calculated. If so, it gets the mean from the cache and skips
## the computation. Otherwise, it calculates the mean of the data and sets
## the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mtrx <- x$getSolve()
  if(!is.null(mtrx)) {
    return(mtrx)
  }
  data <- x$get()
  mtrx <- solve(data, ...)
  x$setSolve(mtrx)
  mtrx
}