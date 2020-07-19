
##Part of Course - R programming from Coursera - Week 3 assignment 

##Function to create matrix object to cache its inverse

##chagned 'm' to 'I' and setmean and getmean to setinverse and getinverse
##from original/provided data
##changed x=numeric() to matrix()
makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) I <<- inverse
  getInverse <- function() I
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  I <- x$getInverse() ##x$getmean() changed to x$getInverse()
  if (!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  mat <- x$get()
  I <- solve(mat, ...)
  x$setInverse(I)
  I
}
