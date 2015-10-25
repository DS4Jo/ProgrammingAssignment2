## cachematrix functions

## 2015/10/24 Assignment2 Coursera R Programming
## These functions are intended to compute the inverse of
## a matrix and caches the results so that an unchanged
## matrix need not be reevaluated.
## NOTE that this assumes the matrix passed as the
## argument is a square matrix and is always invertible.


## makeCacheMatrix creates a special "matrix" object
## that will cache the inverse of the matrix x.

makeCacheMatrix <- function(x = matrix()) {
     m<-NULL
     set<-function(y){
          x<<-y
          m<<-NULL
     }
     get<-function() x
     setmatrixx<-function(solve) m<<- solve
     getmatrixx<-function() m
     list(set=set, get=get,
          setmatrixx=setmatrixx,
          getmatrixx=getmatrixx)
}


## cacheSolve computes the inverse of the special "matrix"
## object returned by makeCacheMatrix (above). If the object x is in
## cache and unchanged, the cached object will be used however if
## it does not exist, will proceed to compute the inverse of 
## the square matrix.

cacheSolve <- function(x, ...) {
     m<-x$getmatrixx()
     if(!is.null(m)){
          message("getting cached data")
          return(m)
     }
     datam <-x$get()
     m<-solve(datam, ...)
     x$setmatrixx(m)
     m
}
