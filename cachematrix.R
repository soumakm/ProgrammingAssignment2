## The following two functions will create a cache for matrix inverse
## and return the cached matrix if it already exists, otherwise calculate
## matrix inverse and set the cache

## The following function returns the cached inverse of a matrix. 
## It also creates a list to cache the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y = matrix()) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv = matrix()) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function will retrieve the inverse of a matrix from cache
## If cache is empty, then it will calculate matrix inverse and se

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
