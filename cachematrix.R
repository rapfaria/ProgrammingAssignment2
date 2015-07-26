## This code creates an a matrix object that is capable of holding
## the value of its inverse matrix. 
## 
## Example usage:
## Suppose you have the following Matrix
##
## > x
##      [,1] [,2] [,3]
## [1,]    2    6    7
## [2,]    4    9    8
## [3,]    3    1   10
##
## you can create a makeCacheMatrix with x
##
## > makeCacheMatrix(x)
##
## From now on (unless you change the matrix), you
## can call cacheSolve to get the inverse of the matrix
##
## > cacheSolve(y)
##            [,1]        [,2]        [,3]
## [1,] -0.8817204  0.56989247  0.16129032
## [2,]  0.1720430  0.01075269 -0.12903226
## [3,]  0.2473118 -0.17204301  0.06451613
##
## If you call cacheSolve again on y, the function
## will return the cached Inverse matrix, as you can see on the message
##
## > cacheSolve(y)
## getting cached matrix
##            [,1]        [,2]        [,3]
## [1,] -0.8817204  0.56989247  0.16129032
## [2,]  0.1720430  0.01075269 -0.12903226
## [3,]  0.2473118 -0.17204301  0.06451613
##
## More information can be found on the following functions


## This creates a new object of a cacheable matrix,
## that is capable of holding it's inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  ## If it is not a matrix, return
  if(!is.matrix(x))
    stop("This function takes a invertible matrix as argument");
  
  ## Set defaults and methods for the new matrix object
  inversematrix <- NULL
  set <- function(y) {
    x <<- y
    inversematrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inversematrix <<- inverse
  getinverse <- function() inversematrix
  
  ##return the new object, which is a list of functions
  ##
  list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## This function is responsible for computing the inverse matrix.
##
## If the inverse matrix has not been computated, it will use the function solve
## to compute the inverse of the matrix, and will cache this result.
##
## If the inverse matrix has been computated before, it will just return
## the previous result using the method getinverse

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  
  ##If the inverse matrix has been computated before, just return it
  if(!is.null(i)) {
    message("getting cached matrix")
    return(i)
  }
  data <- x$get()
  
  ##Compute the inverse matrix, and store it in cache
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
