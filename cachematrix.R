## functions that cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  

  i <- NULL

  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }

  get <- function() {
    m
  }
  
  ##set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ##get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  

  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ##return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  m <- solve(data) %*% data

  x$setInverse(m)
  
  ## Return the matrix
  m
}
