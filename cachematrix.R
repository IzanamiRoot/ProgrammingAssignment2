## Put comments here that give an overall description of what your
## functions do

## This function creates a special object that stores a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set <- function(squared.matrix) {
    x <<- squared.matrix
  }
  get <- function() x
  setmatrixInverse <- function(inverse) m <<- inverse
  getmatrixInverse <- function() m
  list(set = set,
       get = get,
       setinverse = setmatrixInverse,
       getinverse = getmatrixInverse)
}


## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
# test_matrix <- matrix(c(8,5,4,2),2,2)
# test <- makeCacheMatrix(test_matrix)
# print(cacheSolve(test)) 
# print(cacheSolve(test)) 