## These two functions illustrate Lexical Scoping in R. The first function
## calculated the mean of the vector and the second function can access it in 
## order to avoid repetitive calculations.

## This function builds a set of functions and returns them as a list 
## with names. It creates an R object that creates a vector and stores its mean.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y){
    x <<-y
    inverse <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse=getInverse)

}


## Retrieves the mean from the cached value that is stored in the 
## makeCacheMatrix's enviroment. The funtion required an object of type 
## makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}
