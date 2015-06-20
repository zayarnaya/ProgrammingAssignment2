
## Hi everybody!
## What we have here is 2 functions, very similar to those in an example 
## (well, I do not have a great imagination)
## These 2 functions cache the inverse of a matrix (a square one in particular). 
## The first one is basically a set of functions to operate the contains of a 
## given matrix.
## The second one checks if there is a stored value and returns it or computes it.


## The first function, makeCacheMatrix, makes a list of functions. One can use those
## to set a matrix (set), see the stored matrix (get), set an inverted matrix 
## (setsolve) or see the stored inverted matrix (getsolve).
## To use those functions one should make an object by running the makeCacheMatrix 
## function on a square matrix.
## Then one should subset a function to use it.

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  } #sets the stored matrix
  get <- function() x #shows the stored matrix
  setinverse <- function(inverse) inv <<- inverse #sets the inversed matrix
  getinverse <- function() inv #shows the inversed matrix

  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The second function, cacheSolve, checks if there is a cached inversed matrix and 
## if there was no changes made to the "source" matrix. If there is a cached value,
## the function returns it. If there is not any (that might mean that inversed matrix
## is not yet calculated or that the "source" matrix is changed) than the function 
## computes the inversed matrix.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) { 
    message("getting cached data")
    return(inv)
  } #returns the inversed matrix if there is a cached one
  
  message("computing...")
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv #computes the inversed matrix if there isn't any cached
}
     