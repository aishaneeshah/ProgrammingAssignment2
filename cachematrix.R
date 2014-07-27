## These functions are made to cache the inverse of Matrix when it is calculated.
## If the inverse of the same matrix is asked for again, it returns the cached value
## instead of calculating again and thereby reducing the computations required

## makeCacheMatrix returns the list of the functions, which are for setting and 
## getting the value of the matrix and the inverse of the matrix. This list is the
## special type of 'matrix' that will be used to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  getINV <- function() inv
  setINV <- function(inverse) inv <<- inverse
  list(set = set, get = get, getINV = getINV, setINV = setINV)
}


## The function cacheSolve takes in the special 'matrix' returned by the 
## above function. It returns the cached inverse matrix if it is available
## using getINV function. Otherwise, it gets the data from the special 'matrix'
## and computes the inverse using the inbuilt function 'solve'

cacheSolve <- function(x, ...) {
        inv <- x$getINV()
        if(!is.null(inv))
        {
          message('Returning the cached value of inverse')
          return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setINV(inv)
        return(inv)
}
