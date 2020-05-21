## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: creates special matrix containg cached interval matrix of parameter square matrix
## cacheSolve: checks first if it's cached then it return cached interval matrix without calcuation
##             if not, it calcuates and puts in cache through special matrix structure 

## Write a short comment describing this function

## making special matrix with a cached interval matrix
makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set <- function(y){
    x<<-y
    invert<<-NULL
  }
  
  get <-function() x
  setInvert <- function(inv) invert<<-inv
  getInvert <- function() invert
  list(set=set,get=get,setInvert=setInvert,getInvert=getInvert)
  
}


## Write a short comment describing this function

## checks if cached matrix exists, then return it
## Otherwise,it solve interval matrix then cache it
cacheSolve <- function(x, ...) {
  
  invert<-x$getInvert()
  if(!is.null(invert)){
    message("getting cached data")
    return(invert)
  }
  
  data<-x$get()
  invert<- solve(data,...)
  x$setInvert(invert)
  ## Return a matrix that is the inverse of 'x'
  invert
}

