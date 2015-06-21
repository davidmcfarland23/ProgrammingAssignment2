## DM - This function creates a list that stores several functions
##      the input is a square, invertible matrix.
##      set() sets the matrices whereas get() retrieves the input matrix
##      setinverse() sets m to equal the inverse of the matrix
##      getinverse() displays m, i.e., the inverse of our matrix x

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## We need to store the solved matrix and then recall it in
## the below cacheSolve function.
## DM - this function checks to see if the matrix has already been
##      inverted and stored in m. If so, it retrieves m. If not, it
##      solves for m, i.e., the inverse of our matrix x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {                      ##x is a list of functions from above
      message("getting cached data")       ##if m is not null then
      return(m)                            ##returns the cached inverse
    }
    data <- x$get()                        ##else it grabs our original matrix 'x'
    m <- solve(data, ...)                  ##'m' gets the inverse of 'x'
    x$setinverse(m)                        ##we set the inverse for future calls
    m                                      ##finally we return 'm'
  }
