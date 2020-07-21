## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv

  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse
  if(!is.null(inv))
  {
    message("Getting cached data")
    return(inv)
  }
  else
  {
    matriz <- x$get()
    inv <- solve(matriz, ...)
    x$setInverse(inv)
    inv
  }
}

matrizPrueba<- matrix(c(1,3,-5,0,1,-1,-2,-2,9), nrow=3, ncol=3)
cacheSolve(matrizPrueba)
inversaPrueba<- solve(matrizPrueba)

