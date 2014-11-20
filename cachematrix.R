## R function to cache a vector and it's inverse in memory. 
## Part of coursera class rprog-009 

## makeCacheMatrix creates a matrix object that can also cache the inverse
## Usage:
## a<-makeCacheMatrix()
## a$set(y) store matrix
## a$get()  return matrix
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
## Warning:
## The argument to cacheSolve must be a cacheMatrix type list
## The argument matrix must have an inverse matrix

## Usage:
## cacheSolve(x)

cacheSolve <- function(x, ...) {
  
  #Get current inverse value. Also checks if x is a cacheMatrix
  i<-x$getinv()
  
  #return cached inverse if it already exists
  
  if(!is.null(i)) {
    message("cached data")  #output message
    return(i)               #return the inverse
  }
  
  #nothing cached, so calculate and return inverse
  
  datamatrix<-x$get()       #get the matrix
  
  i<-solve(datamatrix,...)  #get the inverse with solve()
  
  x$setinv(i)               #store inverse in the cacheMatrix
  
  i                         #return the inverse
  
}
