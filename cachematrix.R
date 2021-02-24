## Assignment 2: lexical scoping - Caching the inverse of a matrix

## Special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv<- NULL
    set<- function(y){
      x<<- y
      inv<<- NULL
    }
    get<- function() x
    setinverse<- function(inverse) {inv<<- inverse}
    getinverse<- function() {inv}
    list(set= set, get= get,
         setinverse= setinverse,
         getinverse= getinverse)
}


## This function computes the inverse of the special "matrix" returned by the 
## funtion " makeCacheMatrix". 
cacheSolve <- function(x, ...) {
  inv<- x$getinverse() 
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
   mat<- x$get()
   inv<- solve(mat, ...)
   x$setinverse(inv)
   inv ## Return a matrix that is the inverse of 'x'
}
