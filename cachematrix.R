## create a special matrix that contains a function to
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  matr<- NULL
  set<- function(y) {
    
    x<<- y
    matr <<- NULL
    }
  
  get <- function () x
  setinverse <- function(solve) matr<<- solve
  getinverse<- function() matr
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
  
}



## Calculates the inverse of the special matrix created in makecacheMatrix
## It first checks to see if the inverse has already been calculated
## If so, it gets the inverse from the cache and skips the computation
## Otherwise it calculates the inverse of the data and sets the value of the inverse
## in the cahce via the setinverse function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matr<- x$getinverse()
  if(!is.null(matr)) {
    message("Getting cached data")
    return(matr)
  }
  
  data<- x$get()
  matr<- solve(data, ...)
  x$setinverse(matr)
  matr
  
}
