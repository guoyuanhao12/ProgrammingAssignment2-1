
## The following pair of functions caches the value of the inverse of a matrix and stores it elsewhere to be retrieved when needed.

## the makeCacheMatrix function creates a special "matrix" that creates several functions that:
## a) "sets" the value of the matrix with the set function
## b) "gets" or retrieves the value of the matrix with the get function
## c) "sets" the inverse of the matrix with the setinvs function
## d) "gets" or retrieves the inverse of the matrix with the getinvs function


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y) {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinvs<-function(solve) m<<-solve
  getinvs<-function() m
  list(set=set,get=get,setinvs=setinvs,getinvs=getinvs)
}


## The cacheSolve function gets the inverse of the matrix.
## If the matrix has already been inveresed, it retrieves that inverse matrix and returns it
## If the matrix has not already been inveresed, cacheSolve() creates the inverse and stores it in m. 

cacheSolve <- function(x = matrix(), ...) {
  m <- x$getinvs()
  if(!is.null(m)) {
    message ("getting inverse of matrix")
    return (m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setinvs(m)
  m
}
Status API Training Shop Blog About Pricing
© 2016 GitHub, Inc. Terms Privacy Security Contact Help
