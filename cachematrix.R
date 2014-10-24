## Create two functions that can cache the inverse of a matrix.  The first creates the functions to calculate the inverse of the original matrix  
## and the second either calculates or retrieves the inverse matrix


## This function creats a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ##create functions
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ##just pull inverse if it has already been calculated
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##calcualte inverse if necessary
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

##testing to ensure it works
#test1 <- matrix(c(3,4,2,10),2,2)
#test2 <- makeCacheMatrix(test1)
#cacheSolve(test2)