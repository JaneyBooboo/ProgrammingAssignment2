# Assignment: to cache the invere of a square matrix


# creates a special square “matrix” object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  InvertMatrix <- NULL
  set <- function(y) {
    x <<- y
    InvertMatrix <<- NULL
  }
  get <- function() x
  setInvertedMatrix <- function(inverse) InvertMatrix <<- inverse
  getInvertedMatrix <- function() InvertMatrix
  list(set = set, get = get, setInvertedMatrix = setInvertedMatrix, getInvertedMatrix = getInvertedMatrix)
}


## This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  InvertMatrix <- x$getInvertedMatrix()
  if(!is.null(InvertMatrix)) {
    message("getting cached data")
    return(InvertMatrix)
  }
  matrixdata <- x$getInvertedMatrix()
  InvertMatrix <- solve(matrixdata, ...)  # solves for inverse of square matrix
  x$setInvertedMatrix(InvertedMatrix)
  return(InvertMatrix)  # Return a matrix that is the inverse of 'x'
}
        



