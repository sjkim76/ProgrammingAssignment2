## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: creates special matrix containing cached inverse matrix of parameter square matrix
## cacheSolve: checks first if it's cached then it return cached inverse matrix without calcuation
##             if not, it calcuates and puts in cache through special matrix structure 

## Write a short comment describing this function

## this function makes special matrix with a cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  set <- function(y){
    x<<-y
    inverse<<-NULL
  }
  
  get <-function() x
  
  setInverse <- function(inv) inverse<<-inv
  
  getInverse <- function() inverse
  
  list(set=set,get=get,
       setInverse=setInverse,
       getInverse=getInverse)
  
}


## this function checks if cached matrix exists, then return it
## Otherwise,it compute inverse matrix then cache it
cacheSolve <- function(x, ...) {
  
  inverse<-x$getInverse()
  
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  
  data<-x$get()
  
  inverse<- solve(data,...)
  
  x$setInverse(inverse)
  
  ## Return a matrix that is the inverse of 'x'
  inverse
}

