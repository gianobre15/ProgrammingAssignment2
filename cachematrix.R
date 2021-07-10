## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
  n <- NULL   #setting the variable "n" as NULL
  set <- function (y){
    x <<- y
    n <<- NULL 
  }
  get <- function() {x}
  setInverse <- function (inverse) {n <<- inverse}
  getInverse <- function () {n}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {   #Obtaining cache data from given present data
  n <- x$getInverse()
  if (!is.null(n)){                #Checking if the inverse function is NULL
    message("obtaining cache data")  
    return(n)                      #This function returns the inverse value of "n"
  }
  mat <- x$get()
  n <- solve(mat, ...)
  x$setInverse(n)
  n 
  ## Return a matrix that is the inverse of 'x'
}
