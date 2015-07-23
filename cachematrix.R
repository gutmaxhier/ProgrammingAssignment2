makeCacheMatrix <- function(x = matrix()) {
  ##Create a placeholder variable for the inverse matrix
  i <- NULL
  ##Store the input matrix and any cached inverse matrices for reference in the cacheSolve function
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  setinverse <- function(inverse) i <<- inverse
  ##Define the functions with which input matrix and inverse matrix are later retrieved
  get <- function() x
  getinverse <- function() i
  ##Create a list with commands that refer to the specified matrices.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  ##Check if an inverse matrix for the specified(!) input matrix has already been calculated
  ## If yes return it and skip further calculations.
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ##If the inverse matrix isn't cached already, create and return it
  matrixobject <- x$get()
  i <- solve(matrixobject, ...)
  x$setinverse(i)
  i
}

##Test the functions with a random 1000 * 1000 matrix
a<-makeCacheMatrix(x<-matrix(sample(c(-1,0,1),1000000,replace=TRUE),1000,1000))
IAmTheInverseMatrix<-cacheSolve(a)