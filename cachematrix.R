## the function, makeMatrixCache creates a matrix that can cache its inverse
## cacheSolve function computes the inverse of the matrix returned by the 
## makeCaheMatrix above and retrieve the inverse from the cache if it has 
## already been calculated, and has not changed, rather than calculating it each time.

## creating the matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <<- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function()x
    setinvm <- function(invm) m <<- invm
    getinvm <- function() m
    list(set = set, get = get, 
         setinvm = setinvm, 
         getinvm = getinvm)
}


## ## cacheSolve function computes the inverse of the matrix returned by the 
## makeCaheMatrix above and retrieve the inverse from the cache if it has 
## already been calculated, and has not changed, rather than calculating it each time

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    m <- x$getinvm()
    if(!is.null(m)){
      message("Getting cached inverse matrix...")
      return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setinvm(m)
    m
}
