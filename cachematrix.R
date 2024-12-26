## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function creates a special "matrix" object that can cache its inverse.
#It will return a list of functions that can be used to set
#the matrix, get the matrix, set the cached inverse, and get the cached inverse.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize inverse as NULL
  
  # Set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset cached inverse when matrix is set
  }
  
  # Get the matrix
  get <- function() {
    x
  }
  
  # Set the inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Get the cached inverse
  getInverse <- function() {
    inv
  }
  
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}






## Write a short comment describing this function

#cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
#  }

#This function computes the inverse of the matrix stored
#in the special object created by makeCacheMatrix.
#If the inverse is already computed and cached, 
#it retrieves it; otherwise, it computes the inverse 
#using the solve function and stores it in the cache.



cacheSolve <- function(x, ...) {
  # Check if the inverse is already cached
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)  # Return cached inverse if it exists
  }
  
  # If not cached, compute the inverse
  data <- x$get()  # Get the matrix
  inv <- solve(data, ...)  # Compute the inverse using solve
  x$setInverse(inv)  # Cache the inverse
  
  inv  # Return the inverse
}



# Create a special matrix object
matrix_object <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2))

# Compute and cache the inverse
inverse1 <- cacheSolve(matrix_object)
print(inverse1)

# Retrieve the cached inverse (without recalculating)
inverse2 <- cacheSolve(matrix_object)
print(inverse2)


#makeCacheMatrix creates a special matrix object that 
#can store the matrix itself and its inverse.
#cacheSolve checks if the inverse is already cached
#using the getInverse() method. If the inverse is cached,
#it returns it. If not, it computes the inverse using the
#solve() function, stores it using the setInverse() method,
#and then returns the inverse.
