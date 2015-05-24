## Programming Assignment 2
## In this assignment we are going to write an R function that is able to cache potentially time-consuming computations.
## In this case, we will structure two functions that will be used to create a special object that stores a cuadratic matrix
## and caches its inverse.

## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions are used to cache the inverse of a matrix.

## For the first function, makeCacheMatrix creates a special "matrix", this is actually a list containing:
makeCacheMatrix<-function(x=matrix()) {
  inv<-NULL
  set<-function(y) { 
  ## set the value of the matrix
    x<<-y
    inv<<-NULL
  }
  get<-function()x 
  ## get the value of the matrix
  setinverse<-function(solve) inv<<-solve 
  ## set the vakue of the matrix's inverse
  getinverse<-function() inv 
  ## get the value of the matrix's inverse
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## For the first function, it's important to mention that the function "solve()" is used in R to calculate
## the matrix's inverse.

## The second function calculates the inverse of the special "matrix" created with the first function. 
## However, it first checks to see if the inverse has already been calculated (we use "if"). If so, it gets the 
## inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets 
## the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    ## message indicates that we have already calculated the inverse
    return(inv)
  }
  data<-x$get()
  inv<-solve(data, ...)
  x$setinverse(inv)
  inv
}

## Sample run:
> x=matrix(1:4,2,2) ## We stablish the matrix
> m=makeCacheMatrix(x) ## We run the function to cache the inverse of the created object
> m$get() ## We get the matrix stablished above
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> cacheSolve(m) ## We bring the result (inverse)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> 
