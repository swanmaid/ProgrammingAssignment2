## Coursera data science track: R Programming Assignment 2
## This second programming assignment will require you to write an R 
## function that is able to cache potentially time-consuming computations. 
## For example, taking the mean of a numeric vector is typically a fast operation. 
## However, for a very long vector, it may take too long to compute the mean, 
## especially if it has to be computed repeatedly (e.g. in a loop). If the contents 
## of a vector are not changing, it may make sense to cache the value of the mean so 
## that when we need it again, it can be looked up in the cache rather than recomputed. 
## In this Programming Assignment you will take advantage of the scoping rules of the R 
## language and how they can be manipulated to preserve state inside of an R object.

## caching the matrix with get/set and the cached inverse matrix with get/setsolve

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() t(x)
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve either returns the cached inverse matrix or computes the inverse, caches it
## and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(as.matrix(data), ...)
  x$setsolve(m)
  m
}
