## These to functions output the inverse of the square invertible matrix.
## Since this can be a time-consuming operation, calling cacheSolve first checks to see if the
## inverse has already been calculated. If it has, then  the previously cached inverse is returned.
## if it is not, then the inverse is calculated and then cached and output for the user 

# these two functions should be used together, one after another. example:

# foo <- [your favorote invertible matrix here]
# bar <- makeCacheMatrix(foo)
# the_answer <- cacheSolve(bar)


## The makeCacheMatrix function contains four subroutines, each of which is a specific operation on a matrix
## the output is stored as a list of matrices with the list elements being used to

# 1 store a matrix
# 2 retrive the a matrix
# 3 cache the inverse of the matrix
# 4 retrive the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve function outputs the inverse of the matrix output of our previous function
## by either displaying a cached answer or by calculating it if the cached version is empty

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
}
