## this function gives you the inverse matrix of 
## a invertible matrix you input. 

## Its input is a matrix, output is a list containing
## 4 members. This function generally let you input 
## the matrix, or get your inputted matrix, calculate the 
## inverse matrix, or get your calculated inversed matrix.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
}
get <- function() x
setInverse <- function(Inverse) m <<- Inverse
getInverse <- function() m
list(set = set, get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}

## If you already input the matrix and calculated
## the inverse matrix, it will give out the results
## from cache. If not, it will calculate them out 
## and output them.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}