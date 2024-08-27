## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#I don't honestly totally understand what this function is doing
#I just mostly used the example code and modified it to work for our slightly 
#different problem. Vectors and matricies are pretty similar.
#from what I undertand the makeCacheMatrix function creates and enviroment  that
#both the matrix that you provide and it's inverse are stored.

makeCacheMatrix <- function(x = matrix()) {
  # first create the inverse property like in vector example
  inv <- NULL
  
  # then set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the cached inverse when change made to matrix
  } 
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse of the matrix
  getInverse <- function() inv
  
  # Return a list of the above functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
#this is the function that we create to first check to see if the inverse has 
#already been calculated. If it has not then it calculates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()  # Retrieve the cached inverse if it exists
  
  # If the inverse is already cached, we return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If the inverse is not cached, we calculate it
  mat <- x$get()  # Get the matrix
  inv <- solve(mat, ...)  # Compute the inverse using the solve function
  x$setInverse(inv)  # Cache the computed inverse
  
  inv  # Return the inverse
}

#overall these two functions just allow you to store an inverse matrix without
#having to recalculate it if you don't need to.

#testing the fuunctions
# Create a matrix
#m <- matrix(c(1, 2, 3, 4), 2, 2)

# Create the special "matrix" object that can cache its inverse
#cacheMatrix <- makeCacheMatrix(m)

# Compute the inverse of the matrix and cache it
#inverse1 <- cacheSolve(cacheMatrix)

# Retrieve the cached inverse without recalculating
#inverse2 <- cacheSolve(cacheMatrix)

# Print the results
#print(inverse1)
#print(inverse2)
