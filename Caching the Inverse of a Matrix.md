## This function creates a special "matrix" object that can cache its inverse.
## sample is the matrix object that user will submit on the console
makeCacheMatrix <- function(a = matrix()) {
  A <- NULL
  set <- function(b) {
    a <<- b
    A <<- NULL
  }
  get <- function() a
  setinverse <- function(inverse) A <<- inverse
  getinverse <- function() A
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(a, ...) {
  A <- a$getinverse()
  if (!is.null(A)) {
    message("getting cached data")
    return(A)
  }
  data <- a$get()
  A <- solve(data, ...)
  a$setinverse(A)
  A
  
}
## Now I will Give an example which will test that my code is correct.
## We Know that the matrix has inverse if its determenet is non zero. Let's take
## Matrix of Order 3

A <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)
det(A)

## The det of A is 16 it's mean it is a invertible matrix 
B <- makeCacheMatrix(A)
cacheSolve(B)


## Wow it's work. 