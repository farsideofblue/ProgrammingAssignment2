## The following functions create a way to find and store the inverse of a matrix

## This function gives a matrix the basic functions it will need in order to get 
## and store its own inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## create a blank matrix for the inverse
  inv.x <- matrix()
  
  ## define the basic set function for this matrix so that its values can be changed
  set <- function(y) {
    x <<-y
    inv.x <<- matrix()
  }  
  
  ## retrieve the value of the matrix x
  get <- function() x
  
  ## use the solve() function to set the inverse matrix
  setInv <- function(solve) inv.x <<- solve
  
  ## retrieve the value of the inverse matrix
  getInv <- function() inv.x
  
  ## return a list of the defined functions
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function retrieves the inverse matrix. If the inverse matrix has not yet been
## calculated, it will solve for it.

cacheSolve <- function(x, ...) {
  
  ## Retrieve the inverse matrix
  inv.x <- x$getInv()
  
  ## If the first element of the matrix is not set to na (which was the default),
  ## the inverse has previously been cached. Return the inverse matrix.
  if(!is.na(inv.x[1,1])) {    
    message("getting cached inverse")
    return(inv.x)
  } 
  
  ## Otherwise, solve for the inverse matrix, cache it using the setInv function, and
  ## then return the inverse matrix.
  else {
    data <- x$get()
    inv.x <- solve(data, ...)
    x$setInv(inv.x)
    return(inv.x) 
  }
  
}
