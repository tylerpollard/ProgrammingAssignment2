## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes and input matrix and creates a list that contains the 
## functions to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

## cahceSolve first checks to see if the inverse of the matrix has been 
## calculated. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the data and sets the
## value of the inverse matrix in the cache via the setinv() function.

## Write a short comment describing this function

## makeCacheMatrix stores a matrix to input into cacheSolver

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){
    x
  }
  setinv <- function(solve){
    inv <<- solve
  }
  getinv <- function(){
    inv
  }
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv
       )
}


## Write a short comment describing this function

## cacheSolver calculates the inverse of the matrix created with the makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
