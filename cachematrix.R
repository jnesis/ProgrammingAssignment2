# The first function, `makeCacheMatrix` creates a special "matrix", which is
# really a list containing a function to
# 
# 1.  set the value of non-inverted matrix
# 2.  get the value of non-inverted matrix
# 3.  set the inverse of the matrix
# 4.  get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  ## variable "i" stores inverted matrix
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(setMatrix = set, getMatrix = get,
       setInverseMatrix = setinverse,
       getInverseMatrix = getinverse)
}


# The following function inverts the special "matrix"
# created with makeCacheMatrix(). However, it first checks to see if the
# inverse of the matrix has already been calculated. If so, it `get`s the inverted matrix
# from the cache. Otherwise, it calculates the inverse of
# the data and sets the value of the matrix in the cache via the `setInverseMatrix`
# function.

cacheSolve <- function(x, ...) {
    i <- x$getInverseMatrix() ## inverted matrix
    
    ## if the data is cached, return data from cache
    if (!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$getMatrix()
    i <- solve(data)
    x$setInverseMatrix(i)
    i
}

