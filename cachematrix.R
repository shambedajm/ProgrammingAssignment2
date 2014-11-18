## R function to cache a vector and it's inverse in memory. 
## Part of coursera class rprog-009 

## makeCacheMatrix creates a matrix object that can also cache the inverse
## Usage:
## a<-makeCacheMatrix()
## a$set(y) store matrix
## a$get() return matrix
## a$setinv() store inverse
## a$getinv() return inverse

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  
  set <-function(y){
    x<<-y             #using superassignment
    i<<-NULL
  }
  
  get <-function(){
    x                 #return matrix
  }
  
  setinv<-function(inverse){
    i<<-inverse       #using superassignment
  }
  
  getinv<-function(){
    i                 #return inverse of matrix
  }
  
  #create list of each function, named so they can be easily called
  list(set=set, get=get, setinv=setinv,getinv=getinv)
  
}


## CacheSolve will return the inverse of the cacheMatrix argument and store the inverse
## If the inverse does not already exist, it will be caclulated.
## Caveats:
## The argument to cacheSolve must be a cacheMatrix
## The argument matrix must have an inverse matrix

## Usage:
## cacheSolve(x)


cacheSolve <- function(x, ...) {
  #Get current inverse value. Also checks if x is a cacheMatrix
  i<-x$getinv()
  #return cached inverse if it non-null
  if(!is.null(i)) {
    message("cached data")
    return(i)
  }
  #calculate and return inverse
  datamatrix<-x$get()
    i<-solve(datamatrix,...)
  x$setinv(i)
  
  i
  
}
