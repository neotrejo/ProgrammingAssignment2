## makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.
## cacheSolve: Computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.



## The function, makeCacheMatrix creates a list containing a function to
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the matrix inverse
## 4) get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  mat_inv <- NULL
  set <- function(y) {
    x <<- y
    mat_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse_param) mat_inv <<- inverse_param
  getinverse <- function() mat_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function returns the matrix inverse for the data in the list
## created with the makeCacheMatrix function. 
## This function cumputes the matrix inverse if it has not yet been calculated.
## If the matrix was calculated already it simply returns the stored value. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  mat_inv <- x$getinverse()
  if(!is.null(mat_inv)) {
    message("getting cached data")
    return(mat_inv)
  }
  data <- x$get()
  mat_inv <- solve(data)    ## Solve the inverse of a square matrix
  x$setinverse(mat_inv)     ## Cache the inverse of the square matrix
  mat_inv
  
}
