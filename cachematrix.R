
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
## To use those functions one should run the makeCacheMatrix function on a square
## matrix (it works properly if one creates the data set first and use it's name
## as an argument, like this:
## x <- matrix(1:4, 2, 2)
## z <- makeCacheMatrix(x))
## Then one should subset a function to use it, like this:
## z$get() - that would show the contains of x matrix.

### вопрос остался - как напрямую задать матрицу.

makeCacheMatrix <- function(x = matrix()) {
#needs to be done properly - upd так вроде работает
 # changed <- FALSE #это нинада! все равно при изменении инв обнуляетсо
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
    #changed <- TRUE
  }
  get <- function() x #works fine
  setsolve <- function(solve) inv <<- solve #не проверяла upd вроде работает
  #setsolve <- function() {
  #  inv <<- solve(x)
  # 
  #}
    #works fine но надо переделать: солв 
  #будет задаваться в следующей функции, тут просто переменная. либо коннектить его сюда
  getsolve <- function() inv #works fine

  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  
  #if(!is.null(inv) & x == x$get()) { ##тут надо посмотреть что сравниваем
  if(!is.null(inv)) { #вроде работает, но надо проверить условие про неизменность матрицы
    message("getting cached data")
    return(inv)
  }
  #вроде работает
  
  message("computing...")
  data <- x$get()
  inv <- solve(data)
  x$setsolve(inv)
  inv
}
        ## Return a matrix that is the inverse of 'x'



makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}