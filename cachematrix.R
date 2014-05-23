# Cachematrix.r: A pair of functions that cache the inverse of a matrix, using R scoping rules.

# 1. makeCacheMatrix: creates a special "matrix" object that can cache its inverse.

# 2.Function cacheSolve: calculates the inverse of the special "matrix" 
  # created with makeCacheMatrix function. Before to do that it checks 
  # if the inverse has already been calculated:  
      # A. the inverse has already been calculated - it gets   
      # the mean from the cache;
      # B. the inverse has not already been calculated - it computes 
      # the inverse and sets this value in cache.

# Hp. x is a regular, square invertible matrix (we con use solve function to return its inverse)

# 1. Function makeCacheMatrix.
  # Sets the value of the matrix through the <<- operator 
  #(assign a value to an object in an environment that is different from the current environment).
  # Gets the value of the matrix.
  # Sets the inverse of the matrix (solve function).
  # Gets the inverse of the matrix.
  # Creates a list containig the functions defined above.    

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {                
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# 2.Function cacheSolve: calculates the inverse of the special "matrix". 
  # Before it checks to see if the inverse has already been calculated
  # and, if so (and the matrix is not changed), the function retrives
  # the inverse from the cache and skips the computation.
  # Otherwise it calculates the inverse (solve) and sets the value of the   		
  # inverse in cache via setinverse function.
  # Returns a matrix that is the inverse of x.  

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  mydata <- x$get()
  s <- solve(mydata, ...)
  x$setinverse(s)
  s
}
