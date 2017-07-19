##in this programming assignment I have made two functions, 
#these let me cache the inverse of a matrix. 

#First I make sure everything is nice and tidy
rm(list=ls())

#The first function, `makeCacheMatrix` creates a special "vector", which is
#really a list containing a function to
#1.  set the value of the matix
#2.  get the value of the matix
#3.  set the value of the inverse
#4.  get the value of the inverse

#so, here we go!
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

#here is a test/example you can try
#my_matrix <- makeCacheMatrix(matrix(c(4,2,7,6),2,2))

#The next function cacheSolve calculates the inverse of the matrix
#from the above function. However, it first checks to see if the
#inverse has already been calculated. If so, it `get`s the inverse from the
#cache and skips the computation. Otherwise, it calculates the inverse of
#the data and sets the value of the mean in the cache via the `setsolve`
#function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

#and the test/example again
#cacheSolve(my_matrix)
