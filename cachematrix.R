# I write these functions as a requirement of partial fulfillment of Coursera Data Science: R Programming offered in 
#2023

# This is week 3 Assignment.
# GitHub user ID: datahouse-consults
# Student Name: Nicholas Siame Adam
# Year and month taken: 2023/04


# The function creating a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { # defining function argument as "matrix"
  
  inv <- NULL                             # hold value of matrix inverse by initializing inv as NULL; 
   set <- function(y) {                    # re-defining matrix value by setting a function to assign new 
    x <<- y                               # value and resetting inv to NULL for new matrix
    inv <<- NULL              
     }
  get <- function() x                     # defining the get function by returning value of the matrix argument
  
  setinverse <- function(inverse) inv <<- inverse  # assigns value of inv in parent environment
  getinverse <- function() inv                     # gets value of inv where called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) # needed for referring
  # to the functions with the $ operator
}


# Function to compute inverse of the special "matrix" returned by makeCacheMatrix above.

# If the inverse has been calculated already(and the matrix has not changed),
# then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) { # returning matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  
  inv <- solve(data, ...)
  
  x$setinverse(inv)
  
  inv
}