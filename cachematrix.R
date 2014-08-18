## Programming Assingment 2
# For Coursera R programming Course 
# 18-08-2014 by Hiroko Yoshida
# This file contains two functions 

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  M<-x$get()
  m<-solve(M, ...)
  x$setinverse(m)
  m
}


