## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The function makeCacheMatrix create a "matrix", which is really a list containing a function to
##set the value of the vector
##get the value of the vector
##set the value of the mean
##get the value of the mean
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ##Function for Calculating Inverse of the matrix
  calinv <- function(y) {
    x <<-solve(y)
    m <<- NULL
  }
  ##Function for retrieving the value of matrix
  get <- function() x
  ##Function for setting the value of inverse of matrix
  setinv <- function(inverse) m <<- inverse
  ##Function for retrieving the value of inverse of matrix
  getinv <- function() m
  
  list(calinv = calinv, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
#The function cancheSolve calculates the inverse of the special "matrix" created with
##the above function. However, it first checks to see if the inverse has already been 
##calculated. If so, it gets the mean from the cache and skips the computation. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  
  if(!is.null(m)) {
    message("getting invgerse of the matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setmean(m)
  m
}
