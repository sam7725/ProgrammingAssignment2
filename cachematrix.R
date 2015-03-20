## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Make a matrix which can cached its inverse

makeCacheMatrix <- function(x = matrix()) {
  rslt <- NULL
  set <- function(y){
    x <<- y
    rslt <<- NULL
  }
  get <- function() x
  setInv <- function(solve) rslt <<- solve
  getInv <- function() rslt
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  rslt <- x$getInv()
  if(!is.null(rslt)){
    message("getting stored data")
    return(rslt)
  }
  new_matrix <- x$get()
  rslt <- solve(new_matrix, ...)
  x$setInv(rslt)
  rslt
}
