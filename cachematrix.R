##Assuming x is a square invertible matrix.
##First we must set the matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv = null
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
##Second we must get the matrix.
  get = function() x
##Third we set in inverse.
  setinv = function(inverse) inv <<- inverse
##Fourth we get the inverse.
  getinv = function() inv
##Finally we form a list from the previous components.
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

##cacheSolve will then take the set list and return the inverse of matrix 'x'.
cacheSolve <- function(x, ...) {
  inv = x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
}
matrix = x$get()
inv = solve(matrix, ...)

##print the inverse of matrix 'x'
cacheSolve()
