## The following two functions calculate the inverse matrix of a supplied square 
## matrix or retrieve the inverse matrix that has already been produced from the 
## cache. Caching allows speeding up the code by computing the tagret value once 
## and reusing it when it's required without having to calculate it again.

## The makeCacheMatrix function produces a special list that stores the inverted
## matrix. It contains four functions: 1) set, which changes the vector x that's 
## stored in the main function; 2) get, that retrieves the vector x; 3) setinverse,
## which sets the value if the inverse matrix and assigns it to variable m; 4) 
## getinverse, that retrieves the value of the inverse matrix. The list created by 
## this function can used by the cacheSolve function to calculate the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function uses the special matrix produced by makeCacheMatrix to 
## calculate the inverse matrix. Firstly, it checks if the inverse matrix has 
## already been calculated, and if the value is present, it returns it from the
## cache. If it has not been computed yet, the function retrieves the special 
## matrix from makeCacheMatrix and calculates the inverse using the solve operation.
## Notably, it can do that only if the original matrix is invertable.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
