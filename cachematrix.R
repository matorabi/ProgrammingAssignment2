##Since matrix computation (and specifically matrix inversion) is a costly computation,
##it is sometimes a good practice cache the inverse of the matrix rather tham calculating it over an over.
##The objective is to write two functions, namely, 
## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix

## makeCacheMatrix is a function which creates a special "matrix" object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve is a function which computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## ---------------Checking the program------------------------
 m <- matrix(rnorm(16),4,4)
 m1 <- makeCacheMatrix(m)
 cacheSolve(m1)

 # [,1]       [,2]       [,3]       [,4]
 # [1,] 0.02300332  0.3914381  0.7511876  0.2113112
 # [2,] 3.07230298 -0.8506569  0.1098566 -0.3217695
 # [3,] 0.88438639 -0.2075959 -0.6668996 -0.4970044
 # [4,] 1.22333944 -0.4418758  0.2522608 -0.4280278

