## Module cachematrix contains two functions.
## makeCacheMatrix stores a matrix itself and a variable "s" that represents inverse matrix of the first matrix   
## cacheSolve performs calculation of inverse matrix and stores it to "cache"


## Represents the "cache" in context of function environment.
## Contains methods for storing and retrieving the cache and matrix itself
## x is the original matrix
## s is cache variable defined in context of function environment
## return value is the list of of functions for getting and setting the cache and the matrix itself
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Compute the inverse matrix and stores it to the cache 
## x is the "cacheable" object returned by makeCacheMatrix
## ... is some not required additional params for solve function
## return value is the inverse matrix of matrix stored in x
## first we check value of cache (in which we store the inverse matrix)
## if it is not null, we return the cache
## if the cache is not yet created, we calc the inverse matrix, store it to cache and return
cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if (!is.null(s)) {
          message("getting cached data")
          return(s)
        }
        data <- x$get()
        s <- solve(data,b=,...)
        x$setsolve(s)
        ## Return a matrix that is the inverse of 'x'
        s
}
