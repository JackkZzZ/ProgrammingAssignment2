## Get inverse of matrix and read from cache if it has been calculated

## Function of makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  inver<-NULL
  set<-function(y){
    x<<-y
    inver<<-NULL
  }
  get<-function() x
  setinverse<-function(inv) inver<<-inv
  getinverse<-function() inver
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## Get the inverse and read from cache if it is calculated

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver<-x$getinverse()
  if(!is.null(inver)){
    message("getting cached data")
    return(inver)
  }
  m<-x$get()
  inver<-solve(m,...)
  x$setinverse(inver)
  inver
}