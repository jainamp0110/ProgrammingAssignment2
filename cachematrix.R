## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix : store matrix and cache the inverse
#returns : list containing 4 functions 
# 1]set value of matrix
# 2]get value of matrix
# 3]set value as inversed matrix
# 4]get value of inversed matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { #1
    x <<- y
    m <<- NULL
  }
  get <- function() x #2

  setInv <- function(inv) m <<- inv #3
  getInv <- function() m  #4
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function
#cacheSolve : get cached Inverse Matrix, if not calculate it
# Return a matrix that is the inverse of input matrix
cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) { #check if value is cached
    message("getting cached data")
    return(m)
  }
  data <- x$get() #if not, calculate and return the same
  m <- solve(data, ...)
  x$setInv(m)
  m
}
